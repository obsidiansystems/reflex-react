{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module React where

import Prelude hiding ((!!))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Language.Javascript.JSaddle hiding (Ref)
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import GHCJS.Prim.Internal (primToJSVal)
import Data.String

t :: Text -> Text
t = id

tshow :: Show a => a -> Text
tshow = T.pack . show

printJavaScriptException :: JavaScriptException -> JSM ()
printJavaScriptException (JavaScriptException e) = do
  s <- e # t "toString" $ ()
  j <- valToJSON s
  liftIO $ T.putStrLn $ "Exception: " <> tshow j

instance PToJSVal Text where
  pToJSVal s = primToJSVal $ PrimVal_String s

instance IsString JSVal where
  fromString = pToJSVal . T.pack

instance ToJSVal v => ToJSVal (Map Text v) where
  toJSVal m = do
    o@(Object oVal) <- obj
    forM_ (Map.toList m) $ \(k, v) -> do
      (o <# k) =<< toJSVal v
    pure oVal

consoleLog :: ToJSVal a => a -> JSM JSVal
consoleLog x = (global ! t "console") # t "log" $ [x]

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

newtype Element = Element { unElement :: ReaderT React JSM JSVal }

instance IsString Element where
  fromString = Element . pure . pToJSVal . T.pack

newtype Tag = Tag { unTag :: JSVal }

instance IsString Tag where
  fromString = Tag . pToJSVal . T.pack

createElement :: Tag -> Map Text JSVal -> [Element] -> Element
createElement etag props children = Element $ do
  react <- ask
  createdChildren <- mapM unElement children
  lift $ react # t "createElement" $ [pure $ unTag etag, toJSVal props] <> fmap pure createdChildren

fragment :: [Element] -> Element
fragment children = Element $ do
  react <- ask
  fragmentTag <- lift $ fmap Tag $ react ! t "Fragment"
  unElement $ createElement fragmentTag mempty children

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component :: Hook (JSVal -> Render Element) -> ReaderT React JSM (Component JSVal ())
component (Hook hook) = do
  react <- ask
  (callbackId, jsVal) <- lift $ newSyncCallback'' $ \_ _ args -> flip runReaderT react $ do
    render <- hook
    let props = case args of
          [] -> jsUndefined
          arg0 : _ -> arg0
    e <- unRender $ render props
    unElement e
  pure $ Component $ Function callbackId (Object jsVal)

--TODO: Input can be an initializer function rather than value
--TODO: JSVal
--TODO: `set` can take `a -> a` instead of `a`
useState :: (ToJSVal a, FromJSVal a) => a -> Hook (a, a -> JSM ())
useState initialValue = Hook $ do
  react <- ask
  initialJSVal <- lift $ toJSVal initialValue
  result <- lift $ (react # t "useState") initialJSVal
  Just s <- lift $ fromJSVal =<< result !! 0 --TODO: Exception handling
  setter <- lift $ result !! 1
  pure
    ( s
    , \v' -> void $ call setter nullObject [v']
    )

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
