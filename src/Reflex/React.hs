{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Reflex.React
  ( reflexComponent
  ) where

import Prelude hiding ((!!))

import Language.Javascript.JSaddle
import Control.Monad.Except
import Control.Monad.Reader
import qualified GHCJS.DOM.Types as DOM
import Reflex.Dom.Builder.Immediate
import GHCJS.DOM (currentDocumentUnchecked)
import GHCJS.DOM.Document (createDocumentFragment)
import GHCJS.DOM.Node
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Data.IORef
import Reflex.Class
import Foreign.JavaScript.TH
import Reflex.TriggerEvent.Base
import Reflex.Dom.Core
import Control.Concurrent
import Data.Dependent.Sum
import Data.Functor.Identity
import Control.Monad.Ref
import Witherable
import Data.Reflection

import React

type Widget' x js = ImmediateDomBuilderT (SpiderTimeline x) (DomCoreWidget' x js)
-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget' x js = TriggerEventT (SpiderTimeline x) (DomCoreWidget' x js)

type DomCoreWidget' x js = PostBuildT (SpiderTimeline x) (WithJSContextSingleton js (PerformEventT (SpiderTimeline x) (SpiderHost x)))

-- | Turn a Reflex widget into a React component.  It receives `props` from its parent component as a Dynamic JSVal.
reflexComponent
  :: forall props
  .  (JSVal -> JSM props)
  -> (forall x. Given (SpiderTimeline x) => Dynamic (SpiderTimeline x) props -> Widget' x () ())
  -> ReaderT React JSM (Component JSVal ())
reflexComponent decodeProps w = component $ do
  propUpdaterRef <- useRef jsNull
  initialPropsRef <- useRef jsNull
  instantiateWidget <- flip useCallback (Just []) $ \_ _ [eVal] -> withJSContextSingletonMono $ \jsSing -> do
    fromJSVal @DOM.Element eVal >>= \case
      Nothing -> pure () --TODO: This (probably) means we have been destroyed (react gives us `null` here).  Should we do anything about this?
      Just e -> do
        globalDoc <- currentDocumentUnchecked
        eFragment <- createDocumentFragment globalDoc
        initialProps <- decodeProps =<< (initialPropsRef ! t "current")
        propUpdaterJSM <- liftIO $ withSpiderTimeline $ \(timeline :: SpiderTimeline x) -> do
          (gotProps, gotPropsTriggerRef) <- flip runSpiderHostForTimeline timeline newEventWithTriggerRef
          props <- flip runSpiderHostForTimeline timeline $ holdDyn initialProps gotProps
          (events :: Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation], fc) <- attachImmediateWidget' timeline $ \hydrationMode events -> do
            (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
            let go :: DOM.DocumentFragment -> FloatingWidget' x () ()
                go df = do
                  unreadyChildren <- liftIO $ newIORef 0
                  delayed <- liftIO $ newIORef $ pure ()
                  let builderEnv = HydrationDomBuilderEnv
                        { _hydrationDomBuilderEnv_document = globalDoc
                        , _hydrationDomBuilderEnv_parent = Left $ toNode df
                        , _hydrationDomBuilderEnv_unreadyChildren = unreadyChildren
                        , _hydrationDomBuilderEnv_commitAction = pure () --TODO: possibly `replaceElementContents n f`
                        , _hydrationDomBuilderEnv_hydrationMode = hydrationMode
                        , _hydrationDomBuilderEnv_switchover = never
                        , _hydrationDomBuilderEnv_delayed = delayed
                        }
                  lift $ runHydrationDomBuilderT (w props) builderEnv events
            --TODO: Now that we've stopped firefox from processing things while blocked on XHR, we have the problem that re-entering JS from a `liftIO` doesn't work: it blocks because it shouldn't be running inside that synchronous block, but nevertheless it is.
            runWithJSContextSingleton (runPostBuildT (runTriggerEventT (go eFragment) events) postBuild) jsSing
            return (events, postBuildTriggerRef)
          forkIO $ processAsyncEvents' timeline events fc
          pure $ \newPropsRaw -> do
            newProps <- decodeProps newPropsRaw
            liftIO $ do
              mGotPropsTrigger <- readRef gotPropsTriggerRef
              forM_ mGotPropsTrigger $ \gotPropsTrigger -> case fc of
                FireCommand fire -> flip runSpiderHostForTimeline timeline $ do
                  fire [gotPropsTrigger :=> Identity newProps] $ return ()
        (_, propUpdater) <- newSyncCallback'' $ \_ _ [props] -> do
          propUpdaterJSM props
          pure jsUndefined
        propUpdaterRef <# t "current" $ propUpdater
        initialPropsRef <# t "current" $ jsNull
        replaceElementContents e eFragment
    pure jsUndefined
  pure $ \props -> Render $ do
    propUpdater <- lift $ propUpdaterRef ! t "current"
    propUpdaterIsNull <- lift $ valIsNull propUpdater
    case propUpdaterIsNull of
      True -> do
        lift $ initialPropsRef <# t "current" $ props
      False -> do
        _ <- lift $ call propUpdater nullObject [props]
        pure ()
    pure $ createElement "div" ("ref" =: instantiateWidget) ["test"]

{-# INLINABLE attachImmediateWidget' #-}
attachImmediateWidget'
  :: SpiderTimeline x
  -> (   IORef HydrationMode
      -> Chan [DSum (EventTriggerRef (SpiderTimeline x)) TriggerInvocation]
      -> PerformEventT (SpiderTimeline x) (SpiderHost x) (a, IORef (Maybe (EventTrigger (SpiderTimeline x) ())))
     )
  -> IO (a, FireCommand (SpiderTimeline x) (SpiderHost x))
attachImmediateWidget' timeline w = give timeline $ do
  hydrationMode <- liftIO $ newIORef HydrationMode_Immediate
  events <- newChan
  flip runSpiderHostForTimeline timeline $ do
    ((result, postBuildTriggerRef), fc@(FireCommand fire)) <- hostPerformEventT $ w hydrationMode events
    mPostBuildTrigger <- readRef postBuildTriggerRef
    forM_ mPostBuildTrigger $ \postBuildTrigger -> fire [postBuildTrigger :=> Identity ()] $ return ()
    return (result, fc)

processAsyncEvents'
  :: SpiderTimeline x
  -> Chan [DSum (EventTriggerRef t) TriggerInvocation]
  -> FireCommand t (SpiderHost x)
  -> IO ()
processAsyncEvents' timeline events (FireCommand fire) = void $ forkIO $ forever $ do
  ers <- readChan events
  _ <- flip runSpiderHostForTimeline timeline $ do
    mes <- liftIO $ forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
    _ <- fire (catMaybes mes) $ return ()
    liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return ()
