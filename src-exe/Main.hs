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
import Reflex.Dom.Core ((=:))
import Data.Map (Map)
import Control.Monad.Except
import Control.Monad.Reader
import System.IO
import Data.String

import React

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> (`catchError` printJavaScriptException) $ do
    react <- fmap (React . Object) $ arg ! t "react"
    (global <# t "react") $ unReact react
    consoleLog $ unReact react
    comp <- flip runReaderT react $ component $ do
      (v, setV) <- useState (0 :: Int)
      pure $ \myProps -> Render $ do
        myPropsJson <- lift $ fromJSString <$> valToJSON myProps
        (_, onButton) <- lift $ newSyncCallback'' $ \_ _ _ -> flip runReaderT react $ do
          lift $ setV $ v + 1
          pure jsUndefined
        let buttonProps = mconcat
              [ "onClick" =: onButton
              ]
        pure $ fragment
          [ createElement "strong" mempty ["Props: ", fromString myPropsJson, "; State: ", fromString $ show v]
          , createElement "button" buttonProps ["Test"]
          ]
    _ <- (arg # t "setVal") ["comp" =: pToJSVal comp :: Map Text JSVal]
    pure ()
