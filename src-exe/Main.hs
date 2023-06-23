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
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Prelude hiding ((!!))

import Data.Text (Text)
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JSaddle hiding (Ref)
import Reflex.Dom.Main
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import System.IO
import Data.String
import GHCJS.Prim.Internal (primToJSVal)
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

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> (`catchError` printJavaScriptException) $ do
    react <- fmap (React . Object) $ arg ! t "react"
    (global <# t "react") $ unReact react
    consoleLog $ unReact react
    comp <- flip runReaderT react $ reflexComponent $ \gotProps -> do
      display =<< count gotProps
    _ <- (arg # t "setVal") [Map.singleton "comp" (pToJSVal comp) :: Map Text JSVal]
    pure ()

simpleComponent :: ReaderT React JSM (Component JSVal ())
simpleComponent = do
  react <- ask
  component $ do
    (v, setV) <- useState (0 :: Int)
    onButton <- useCallback (\_ _ _ -> flip runReaderT react $ do
        lift $ setV $ v + 1
        pure jsUndefined) (Just [primToJSVal $ PrimVal_Number $ fromIntegral v])
    pure $ \myProps -> Render $ do
      myPropsJson <- lift $ fromJSString <$> valToJSON myProps
      let buttonProps = mconcat
            [ Map.singleton "onClick" onButton
            ]
      pure $ fragment
        [ createElement "strong" mempty ["Props: ", fromString myPropsJson, "; State: ", fromString $ show v]
        , createElement "button" buttonProps ["Test"]
        ]

type Widget' x js = ImmediateDomBuilderT (SpiderTimeline x) (DomCoreWidget' x js)
-- | A widget that isn't attached to any particular part of the DOM hierarchy
type FloatingWidget' x js = TriggerEventT (SpiderTimeline x) (DomCoreWidget' x js)

type DomCoreWidget' x js = PostBuildT (SpiderTimeline x) (WithJSContextSingleton js (PerformEventT (SpiderTimeline x) (SpiderHost x)))

--TODO: Each instance should be a separate reflex timeline
reflexComponent :: (forall x. Given (SpiderTimeline x) => Event (SpiderTimeline x) JSVal -> Widget' x () ()) -> ReaderT React JSM (Component JSVal ())
reflexComponent w = component $ do
  propUpdaterRef <- useRef jsNull
  instantiateWidget <- flip useCallback (Just []) $ \_ _ [eVal] -> withJSContextSingletonMono $ \jsSing -> do
    fromJSVal @DOM.Element eVal >>= \case
      Nothing -> pure () --TODO: This (probably) means we have been destroyed (react gives us `null` here).  Should we do anything about this?
      Just e -> do
        globalDoc <- currentDocumentUnchecked
        eFragment <- createDocumentFragment globalDoc
        propUpdaterIO <- liftIO $ withSpiderTimeline $ \(timeline :: SpiderTimeline x) -> do
          (gotProps, gotPropsTriggerRef) <- flip runSpiderHostForTimeline timeline newEventWithTriggerRef
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
                  lift $ runHydrationDomBuilderT (w gotProps) builderEnv events
            runWithJSContextSingleton (runPostBuildT (runTriggerEventT (go eFragment) events) postBuild) jsSing
            return (events, postBuildTriggerRef)
          forkIO $ processAsyncEvents' timeline events fc
          pure $ \props -> do
            mGotPropsTrigger <- readRef gotPropsTriggerRef
            forM_ mGotPropsTrigger $ \gotPropsTrigger -> case fc of
              FireCommand fire -> flip runSpiderHostForTimeline timeline $ do
                fire [gotPropsTrigger :=> Identity props] $ return ()
        (_, propUpdater) <- newSyncCallback'' $ \_ _ [props] -> do
          liftIO $ propUpdaterIO props
          pure jsUndefined
        propUpdaterRef <# t "current" $ propUpdater
        replaceElementContents e eFragment
    pure jsUndefined
  pure $ \props -> Render $ do
    propUpdater <- lift $ propUpdaterRef ! t "current"
    propUpdaterIsNull <- lift $ valIsNull propUpdater
    when (not propUpdaterIsNull) $ do
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
