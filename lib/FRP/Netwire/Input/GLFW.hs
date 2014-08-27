module FRP.Netwire.Input.GLFW (
  GLFWInputCtl,
) where

--------------------------------------------------------------------------------
import qualified Data.Set as Set
import qualified Graphics.UI.GLFW as GLFW
import Control.Concurrent.STM
import GHC.Float hiding (clamp)
--------------------------------------------------------------------------------

clamp :: Ord a => a -> a -> a -> a
clamp x a b = if x < a then a else if x > b then b else x

newRange :: Floating a => a -> (a, a) -> (a, a) -> a
newRange x (omin, omax) (nmin, nmax) =
  nmin + (nmax - nmin) * ((x - omin) / (omax - omin))

newRangeC :: (Ord a, Floating a) => a -> (a, a) -> (a, a) -> a
newRangeC x o n@(nmin, nmax) = clamp (newRange x o n) nmin nmax

data GLFWInput = GLFWInput {
  keysPressed :: Set.Set GLFW.Key,
  mbPressed :: Set.Set GLFW.MouseButton,
  cursor :: Maybe (Float, Float),
  misc :: [Int]
} deriving(Show)

instance Key GLFW.Key
instance MouseButton GLFW.MouseButton

-- newtype GLFWInputT m a = GLFWInputT {
--  runInput :: MonadInput GLFW.Key m => GLFWInput -> m b -> IO (
--
-- instance 

kEmptyInput :: GLFWInput
kEmptyInput = GLFWInput { keysPressed = Set.empty,
                          mbPressed = Set.empty,
                          cursor = Nothing,
                          misc = [] }

isKeyPressed :: GLFW.Key -> GLFWInput -> Bool
isKeyPressed key = (Set.member key) . keysPressed

withPressedKey :: GLFWInput -> GLFW.Key -> (a -> a) -> a -> a
withPressedKey input key fn = if isKeyPressed key input then fn else id

debounceKey :: GLFW.Key -> GLFWInput -> GLFWInput
debounceKey key = (\input -> input { keysPressed = Set.delete key (keysPressed input) })

isButtonPressed :: GLFW.MouseButton -> GLFWInput -> Bool
isButtonPressed mb = (Set.member mb) . mbPressed

withPressedButton :: GLFWInput -> GLFW.MouseButton -> (a -> a) -> a -> a
withPressedButton input mb fn = if isButtonPressed mb input then fn else id

debounceButton :: GLFW.MouseButton -> GLFWInput -> GLFWInput
debounceButton mb = (\input -> input { mbPressed = Set.delete mb (mbPressed input) })

data GLFWInputControl = IptCtl (TVar GLFWInput) GLFW.Window

-- Returns a snapshot of the input
setCursorToWindowCenter :: GLFW.Window -> IO ()
setCursorToWindowCenter win = do
  (w, h) <- GLFW.getWindowSize win
  GLFW.setCursorPos win (fromIntegral w / 2.0) (fromIntegral h / 2.0)

getInput :: GLFWInputControl -> IO(GLFWInput)
getInput (IptCtl var _) = readTVarIO var

setGLFWInput :: GLFWInputControl -> GLFWInput -> IO ()
setGLFWInput (IptCtl var win) ipt = do
  case (cursor ipt) of
    Just _ -> return ()
    Nothing -> setCursorToWindowCenter win
  atomically $ writeTVar var ipt

resetCursorPos :: GLFWInput -> GLFWInput
resetCursorPos = (\input -> input { cursor = Nothing })

--------------------------

{--
scrollCallback :: GLFWInputControl -> GLFW.Window -> Double -> Double -> IO ()
scrollCallback (IptCtl ctl _) _ xoff yoff = atomically $ modifyTVar' ctl updateScroll
  where
    updateScroll :: GLFWInput -> GLFWInput
    updateScroll =
      (\input -> input { misc = (Scroll xoff yoff) : (filter notScroll $ misc input) })

    notScroll :: MiscGLFWInput -> Bool
    notScroll (Scroll _ _) = False
    notScroll _ = True
--}

keyCallback :: GLFWInputControl -> GLFW.Window ->
               GLFW.Key -> Int -> GLFW.KeyState -> GLFW.ModifierKeys -> IO ()
keyCallback (IptCtl ctl _) _ key _ keystate _ = atomically $ modifyTVar' ctl modifyKeys
  where
    updateKeys :: (Set.Set GLFW.Key -> Set.Set GLFW.Key) -> GLFWInput -> GLFWInput
    updateKeys fn = (\input -> input { keysPressed = fn (keysPressed input) })

    modifyKeys :: GLFWInput -> GLFWInput
    modifyKeys = case keystate of
      GLFW.KeyState'Pressed -> updateKeys $ Set.insert key
      GLFW.KeyState'Released -> updateKeys $ Set.delete key
      _ -> id

mouseButtonCallback :: GLFWInputControl -> GLFW.Window ->
                       GLFW.MouseButton -> GLFW.MouseButtonState ->
                       GLFW.ModifierKeys -> IO ()
mouseButtonCallback (IptCtl ctl _) _ button state _ =
  atomically $ modifyTVar' ctl modify
  where
    update :: (Set.Set GLFW.MouseButton -> Set.Set GLFW.MouseButton) ->
              GLFWInput -> GLFWInput
    update fn = (\ipt -> ipt { mbPressed = fn (mbPressed ipt) })

    modify :: GLFWInput -> GLFWInput
    modify = case state of
      GLFW.MouseButtonState'Pressed -> update $ Set.insert button
      GLFW.MouseButtonState'Released -> update $ Set.delete button

-- !HACK! Right now we're simply setting the cursor position as disabled
-- regardless of application ... we should really expose this to the user
-- somehow...

cursorPosCallback :: GLFWInputControl -> GLFW.Window -> Double -> Double -> IO ()
cursorPosCallback (IptCtl ctl _) win x y = do
  (w, h) <- GLFW.getWindowSize win
  let xf = newRangeC (double2Float x) (0, fromIntegral w) (-1, 1)
      yf = newRangeC (double2Float y) (0, fromIntegral h) (-1, 1)
  atomically $ modifyTVar' ctl (\ipt -> ipt { cursor = Just (xf, yf)})

mkInputControl :: GLFW.Window -> IO (GLFWInputControl)
mkInputControl win = do

  GLFW.setCursorInputMode win GLFW.CursorInputMode'Disabled

  ctlvar <- newTVarIO kEmptyInput
  let ctl = IptCtl ctlvar win
  -- GLFW.setScrollCallback win (Just $ scrollCallback ctl)
  GLFW.setKeyCallback win (Just $ keyCallback ctl)
  GLFW.setCursorPosCallback win (Just $ cursorPosCallback ctl)
  GLFW.setMouseButtonCallback win (Just $ mouseButtonCallback ctl)
  return ctl

pollGLFW :: GLFWInput -> GLFWInputControl -> IO (GLFWInput)
pollGLFW ipt iptctl = do
  setGLFWInput iptctl ipt
  GLFW.pollEvents
  getInput iptctl
