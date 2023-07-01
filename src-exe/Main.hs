{-# LANGUAGE CPP #-}
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
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Prelude hiding ((!!))

import Data.Text (Text)
import Language.Javascript.JSaddle hiding (Ref)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Except
import Control.Monad.Reader

import Example.SimpleProps
import Example.SimpleState

#ifndef ghcjs_HOST_OS
import Language.Javascript.JSaddle.Warp
#endif

import React

main :: IO ()
main = exportToJS $ sequence $ Map.fromList
  [ ( "simplePropsHaskell"
    , lift . toJSVal =<< simplePropsHaskell
    )
  , ( "simplePropsReflex"
    , lift . toJSVal =<< simplePropsReflex
    )
  , ( "simpleStateHaskell"
    , lift . toJSVal =<< simpleStateHaskell
    )
  , ( "simpleStateReflex"
    , lift . toJSVal =<< simpleStateReflex
    )
  ]

exportToJS :: ReaderT React JSM (Map Text JSVal) -> IO ()
exportToJS build = do
  let port = 3001 --TODO: Get this from npm config or something
  runJS port $ \arg -> do
    react <- fmap (React . Object) $ arg ! t "react"
    m <- flip runReaderT react build
    _ <- (arg # t "setVal") [m]
    pure ()

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe "getReactAndSetHaskell"
    getReactAndSetHaskell :: JSM (JSVal)

runJS _ f = f =<< getReactAndSetHaskell

#else

runJS :: Int -> (JSVal -> JSM ()) -> IO ()
runJS port f = do
  run port $ \arg -> f arg `catchError` printJavaScriptException

#endif
