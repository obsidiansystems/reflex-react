{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
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
import Reflex.Dom.Core ((=:))
import qualified GHCJS.DOM.Types as DOM
import Reflex.Dom.Builder.Immediate
import GHCJS.DOM (currentDocumentUnchecked)
import qualified GHCJS.DOM as DOM
import GHCJS.DOM.Document (createDocumentFragment)
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.DOM.NonElementParentNode
import GHCJS.DOM.Types (JSM)
import Reflex.Host.Class
import Reflex.PerformEvent.Base
import Reflex.PostBuild.Base
import Data.IORef
import Reflex.Class
import Foreign.JavaScript.TH
import Reflex.TriggerEvent.Base
import Reflex.TriggerEvent.Class
import Reflex.Dom.Core
import Control.Concurrent

import React

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> (`catchError` printJavaScriptException) $ do
    react <- fmap (React . Object) $ arg ! t "react"
    (global <# t "react") $ unReact react
    consoleLog $ unReact react
    comp <- flip runReaderT react $ reflexComponent $ do
      display =<< count =<< button "Test"
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

reflexComponent :: Widget () () -> ReaderT React JSM (Component JSVal ())
reflexComponent w = component $ do
  ref <- flip useCallback (Just []) $ \_ _ [eVal] -> withJSContextSingletonMono $ \jsSing -> do
    Just e <- fromJSVal @DOM.Element eVal
    globalDoc <- currentDocumentUnchecked
    eFragment <- createDocumentFragment globalDoc
    (events, fc) <- liftIO . attachImmediateWidget $ \hydrationMode events -> do
      (postBuild, postBuildTriggerRef) <- newEventWithTriggerRef
      let go :: DOM.DocumentFragment -> FloatingWidget () ()
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
            lift $ runHydrationDomBuilderT w builderEnv events
      runWithJSContextSingleton (runPostBuildT (runTriggerEventT (go eFragment) events) postBuild) jsSing
      return (events, postBuildTriggerRef)
    replaceElementContents e eFragment
    liftIO $ forkIO $ processAsyncEvents events fc
    pure jsUndefined
  pure $ \props -> Render $ do
    pure $ createElement "div" ("ref" =: ref) ["test"]
