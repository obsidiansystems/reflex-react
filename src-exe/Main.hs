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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader
import System.IO
import Data.String
import GHCJS.Prim.Internal (primToJSVal)
import Reflex.Class
import Reflex.Dom.Core
import Control.Concurrent

import React
import Reflex.React

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  provideJS $ sequence $ Map.fromList
    [ ( "reflex"
      , fmap pToJSVal $ reflexComponent $ \props -> do
          display =<< count (updated props)
      )
    , ( "simple"
      , fmap pToJSVal simpleComponent
      )
    ]

provideJS :: ReaderT React JSM (Map Text JSVal) -> IO ()
provideJS build = do
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> (`catchError` printJavaScriptException) $ do
    react <- fmap (React . Object) $ arg ! t "react"
    m <- flip runReaderT react build
    _ <- (arg # t "setVal") [m]
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
      pure $ createFragment
        [ createElement "strong" mempty ["Props: ", fromString myPropsJson, "; State: ", fromString $ show v]
        , createElement "button" buttonProps ["Test"]
        ]
