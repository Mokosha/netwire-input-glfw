module FRP.Netwire.Input.GLFW (
  GLFWInput, GLFWInputT,
  
) where

--------------------------------------------------------------------------------
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative
import Control.Concurrent.STM
import Control.Monad.State
import GHC.Float hiding (clamp)

import FRP.Netwire.Input
--------------------------------------------------------------------------------

clamp :: Ord a => a -> a -> a -> a
clamp x a b = if x < a then a else if x > b then b else x

newRange :: Floating a => a -> (a, a) -> (a, a) -> a
newRange x (omin, omax) (nmin, nmax) =
  nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

newRangeC :: (Ord a, Floating a) => a -> (a, a) -> (a, a) -> a
newRangeC x o n@(nmin, nmax) = clamp (newRange x o n) nmin nmax

data GLFWInputState = GLFWInputState {
  keysPressed :: Set.Set GLFW.Key,
  mbPressed :: Set.Set GLFW.MouseButton,
  cursorPos :: Maybe (Float, Float),
  cursorMode :: CursorMode,
  scrollAmt :: (Double, Double)
} deriving(Show)

instance Key GLFW.Key
instance MouseButton GLFW.MouseButton

-- !FIXME! Perhaps this is better in its own newtype
type GLFWInput a = State GLFWInputState a
type GLFWInputT m a = StateT GLFWInputState m a

instance (Functor m, Monad m) =>
         MonadInput GLFW.Key GLFW.MouseButton (StateT GLFWInputState m) where

  keyIsPressed :: GLFW.Key -> StateT GLFWInputState m Bool
  keyIsPressed key = get >>= (return . isKeyPressed key)

  releaseKey :: GLFW.Key -> StateT GLFWInputState m ()
  releaseKey key = get >>= (put . debounceKey key)

  mbIsPressed :: GLFW.MouseButton -> StateT GLFWInputState m Bool
  mbIsPressed mb = get >>= (return . isButtonPressed mb)

  releaseButton :: GLFW.MouseButton -> StateT GLFWInputState m ()
  releaseButton mb = get >>= (put . debounceButton mb)

  cursor :: StateT GLFWInputState m (Float, Float)
  cursor = do
    ipt <- get
    case cursorPos ipt of
      Just (x, y) -> return (x, y)
      Nothing -> return (0, 0)

  setCursorMode :: CursorMode -> StateT GLFWInputState m ()
  setCursorMode mode = do
    ipt <- get
    put (ipt { cursorMode = mode })

  scroll :: StateT GLFWInputState m (Double, Double)
  scroll = get >>= (return . scrollAmt)

kEmptyInput :: GLFWInputState
kEmptyInput = GLFWInputState { keysPressed = Set.empty,
                               mbPressed = Set.empty,
                               cursorPos = Nothing,
                               cursorMode = CursorMode'Enabled,
                               scrollAmt = (0, 0) }

isKeyPressed :: GLFW.Key -> GLFWInputState -> Bool
isKeyPressed key = (Set.member key) . keysPressed

withPressedKey :: GLFWInputState -> GLFW.Key -> (a -> a) -> a -> a
withPressedKey input key fn = if isKeyPressed key input then fn else id

debounceKey :: GLFW.Key -> GLFWInputState -> GLFWInputState
debounceKey key = (\input -> input { keysPressed = Set.delete key (keysPressed input) })

isButtonPressed :: GLFW.MouseButton -> GLFWInputState -> Bool
isButtonPressed mb = (Set.member mb) . mbPressed

withPressedButton :: GLFWInputState -> GLFW.MouseButton -> (a -> a) -> a -> a
withPressedButton input mb fn = if isButtonPressed mb input then fn else id

debounceButton :: GLFW.MouseButton -> GLFWInputState -> GLFWInputState
debounceButton mb = (\input -> input { mbPressed = Set.delete mb (mbPressed input) })

data InputGLFWControl = IptCtl (TVar GLFWInputState) GLFW.Window

-- Returns a snapshot of the input
setCursorToWindowCenter :: GLFW.Window -> IO ()
setCursorToWindowCenter win = do
  (w, h) <- GLFW.getWindowSize win
  GLFW.setCursorPos win (fromIntegral w / 2.0) (fromIntegral h / 2.0)

getInput :: InputGLFWControl -> IO(GLFWInputState)
getInput (IptCtl var _) = readTVarIO var

setInput :: InputGLFWControl -> GLFWInputState -> IO ()
setInput (IptCtl var win) ipt = do
  case (cursorPos ipt) of
    Just _ -> return ()
    Nothing -> setCursorToWindowCenter win
  atomically $ writeTVar var (ipt { scrollAmt = (0, 0) })

resetCursorPos :: GLFWInputState -> GLFWInputState
resetCursorPos = (\input -> input { cursorPos = Nothing })

--------------------------

scrollCallback :: InputGLFWControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback (IptCtl ctl _) _ xoff yoff = atomically $ modifyTVar' ctl updateScroll
  where
    updateScroll :: GLFWInputState -> GLFWInputState
    updateScroll = (\input -> input { scrollAmt = (xoff, yoff) })

keyCallback :: InputGLFWControl -> GLFW.Window ->
               GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback (IptCtl ctl _) _ key _ keystate _ = atomically $ modifyTVar' ctl modifyKeys
  where
    updateKeys :: (Set.Set GLFW.Key -> Set.Set GLFW.Key) -> GLFWInputState -> GLFWInputState
    updateKeys fn = (\input -> input { keysPressed = fn (keysPressed input) })

    modifyKeys :: GLFWInputState -> GLFWInputState
    modifyKeys = case keystate of
      GLFW.KeyState'Pressed -> updateKeys $ Set.insert key
      GLFW.KeyState'Released -> updateKeys $ Set.delete key
      _ -> id

mouseButtonCallback :: InputGLFWControl -> GLFW.Window ->
                       GLFW.MouseButton -> GLFW.MouseButtonState ->
                       GLFW.ModifierKeys -> IO ()
mouseButtonCallback (IptCtl ctl _) _ button state _ =
  atomically $ modifyTVar' ctl modify
  where
    update :: (Set.Set GLFW.MouseButton -> Set.Set GLFW.MouseButton) ->
              GLFWInputState -> GLFWInputState
    update fn = (\ipt -> ipt { mbPressed = fn (mbPressed ipt) })

    modify :: GLFWInputState -> GLFWInputState
    modify = case state of
      GLFW.MouseButtonState'Pressed -> update $ Set.insert button
      GLFW.MouseButtonState'Released -> update $ Set.delete button

-- !HACK! Right now we're simply setting the cursor position as disabled
-- regardless of application ... we should really expose this to the user
-- somehow...

cursorPosCallback :: InputGLFWControl -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback (IptCtl ctl _) win x y = do
  (w, h) <- GLFW.getWindowSize win
  let xf = newRangeC (double2Float x) (0, fromIntegral w) (-1, 1)
      yf = newRangeC (double2Float y) (0, fromIntegral h) (-1, 1)
  atomically $ modifyTVar' ctl (\ipt -> ipt { cursorPos = Just (xf, yf)})

mkInputControl :: GLFW.Window -> IO (InputGLFWControl)
mkInputControl win = do

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  ctlvar <- newTVarIO kEmptyInput
  let ctl = IptCtl ctlvar win
  GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  GLFW.setKeyCallback win (Just $ keyCallback ctl)
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback ctl)
  GLFW.setMouseButtonCallback win (Just $ mouseButtonCallback ctl)
  return ctl

pollGLFW :: GLFWInputState -> InputGLFWControl -> IO (GLFWInputState)
pollGLFW ipt iptctl = do
  setInput iptctl ipt
  GLFW.pollEvents
  getInput iptctl
