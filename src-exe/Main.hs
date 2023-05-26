module Main where

import Data.Text

main :: IO ()
main = putStrLn "Hello, Haskell!"

newtype Hook a = Hook { unHook :: IO a }
  deriving (Functor, Applicative)

data Component props refVal

--TODO: The Hook section shouldn't have any control flow to it; probably it also shouldn't depend on props except in specific ways
component :: (props -> Hook (Render ())) -> Component props ()
component = undefined

--TODO: Input can be an initializer function rather than value
--TODO: JSVal
useState :: a -> Hook (a, SetFunction a)
useState = undefined

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

newtype Render a = Render { unRender :: IO a }
  deriving (Functor, Applicative, Monad)

newtype Effect a = Effect { unEffect :: IO a }
  deriving (Functor, Applicative, Monad)
