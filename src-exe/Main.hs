{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main where

import Prelude hiding ((!!))

import Data.Text (Text)
import qualified Data.Text as T
import Language.Javascript.JSaddle.Warp
import Language.Javascript.JSaddle hiding (Ref)
--import Reflex.Dom.Main
import Reflex.Dom.Core
--import Reflex
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader
import System.IO

t :: Text -> Text
t = id

tshow :: Show a => a -> Text
tshow = T.pack . show

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  let port = 3001 --TODO: Get this from npm config or something
  run port $ \arg -> do
    react <- fmap (React . Object) $ arg ! t "react"
    comp <- flip runReaderT react $ component $ do
      _ <- Hook $ lift $ (global ! t "console") # t "log" $ [t ""]
      pure $ \myProps -> Render $ do
        myPropsJson <- lift $ valToJSON myProps
        lift $ (global ! t "console") # t "log" $ [toJSVal myPropsJson]
    call comp arg [t "test"]
    pure ()

instance ToJSVal v => ToJSVal (Map Text v) where
  toJSVal m = do
    o@(Object oVal) <- obj
    forM_ (Map.toList m) $ \(k, v) -> do
      (o <# k) =<< toJSVal v
    pure oVal

instance ToJSVal (Component props refVal) where
  toJSVal (Component f) = toJSVal f

instance PToJSVal (Component props refVal) where
  pToJSVal (Component f) = pToJSVal f

instance PToJSVal Function where
  pToJSVal (Function _ o) = pToJSVal o

instance PToJSVal Object where
  pToJSVal (Object v) = v

newtype Hook a = Hook { unHook :: ReaderT React JSM a }
  deriving (Functor, Applicative, Monad)

newtype Component props refVal = Component { unComponent :: Function }

-- | An object that contains the React library
newtype React = React { unReact :: Object }

instance MakeObject React where
  makeObject = pure . unReact

instance MakeObject (Component props refVal) where
  makeObject = makeObject . functionObject . unComponent

createElement :: Text -> Object -> [JSM JSVal] -> ReaderT React JSM JSVal
createElement etag props children = do
  react <- ask
  lift $ react # t "createElement" $ (etag, props, children)

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component :: Hook (JSVal -> Render JSVal) -> ReaderT React JSM (Component JSVal ())
component (Hook hook) = do
  react <- ask
  (callbackId, jsVal) <- lift $ newSyncCallback'' $ \_ _ args -> flip runReaderT react $ do
    render <- hook
    let props = case args of
          [] -> jsUndefined
          arg0 : _ -> arg0
    unRender $ render props
  pure $ Component $ Function callbackId (Object jsVal)

--TODO: Input can be an initializer function rather than value
--TODO: JSVal
useState :: JSVal -> Hook (JSVal, JSVal)
useState _initialValue = Hook $ do
  react <- ask
  _result <- lift $ (react # t "useState") jsUndefined
  pure (jsUndefined, jsUndefined) {-
  s <- lift $ result !! 0
  setter <- lift $ result !! 1
  pure (s, setter)
--}
--TODO: Can be called during rendering https://react.dev/reference/react/useState#storing-information-from-previous-renders but "shouldn't" generally
type SetFunction a = Either a (a -> a) -> Effect ()

useReducer :: Reducer s a -> a -> Maybe (a -> a) -> Hook (a, DispatchFunction a)
useReducer = undefined

type DispatchFunction a = a -> Effect ()

type Reducer s a = s -> a -> s

useContext :: Context a -> Hook a
useContext = undefined

data Context a

createContext :: a -> IO (Context a)
createContext = undefined

provider :: Context a -> a -> Render b -> Render b
provider = undefined

useRef :: a -> Hook (Ref a)
useRef = undefined

data Ref a

forwardRef :: (props -> Ref refVal -> Hook (Render ())) -> Component props refVal
forwardRef = undefined

useImperativeHandle :: Ref a -> Effect a -> Maybe [Dep] -> Hook ()
useImperativeHandle = undefined

data Dep

useEffect :: IO (IO ()) -> Maybe [Dep] -> Hook ()
useEffect = undefined

useMemo :: a -> Maybe [Dep] -> Hook ()
useMemo = undefined

useCallback :: a -> Maybe [Dep] -> Hook ()
useCallback = useMemo --TODO: Is there actually any difference?

useTransition :: Hook (Bool, Effect () -> Effect ())
useTransition = undefined

useDeferredValue :: a -> Hook a
useDeferredValue = undefined

useDebugValue :: a -> Maybe (a -> b) -> Hook ()
useDebugValue = undefined

useId :: Hook Text
useId = undefined

useSyncExternalStore :: (IO () -> IO (IO ())) -> IO a -> Maybe (IO a) -> Hook ()
useSyncExternalStore = undefined

newtype Render a = Render { unRender :: ReaderT React JSM a }
  deriving (Functor, Applicative, Monad)

newtype Effect a = Effect { unEffect :: JSM a }
  deriving (Functor, Applicative, Monad)
