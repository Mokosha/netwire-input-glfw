module Main where

--------------------------------------------------------------------------------
import Control.Monad.State
import Control.Monad.Trans.Class
import Control.Wire hiding (unless)

import Data.Array.IO
import Data.Array.Storable
import Data.Word

import FRP.Netwire.Input
import FRP.Netwire.Input.GLFW

import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL

import Foreign.Storable
import Foreign.Ptr

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--
-- Boilerplate

data Shape = Shape GL.NumArrayIndices GL.BufferObject GL.BufferObject

mkCircle :: IO (Shape)
mkCircle = let
  slices = 50
  vptrsize = toEnum $ (slices + 2) * (sizeOf (undefined :: GL.Vertex2 Float))
  iptrsize = toEnum $ (slices + 2) * (sizeOf (undefined :: Int))
  winding = [0,(2*pi/(fromIntegral slices))..] :: [Float]
  in do
    vbuf <- GL.genObjectName
    GL.bindBuffer GL.ArrayBuffer GL.$= (Just vbuf)
    let vl = (GL.Vertex2 0 0) : (zipWith GL.Vertex2 (map cos winding) (map sin winding))
    verts <- newListArray (0, slices + 1) vl
    withStorableArray verts
      (\ptr -> GL.bufferData GL.ArrayBuffer GL.$= (vptrsize, ptr, GL.StaticDraw))

    ibuf <- GL.genObjectName
    GL.bindBuffer GL.ElementArrayBuffer GL.$= (Just ibuf)
    idxs <- newListArray (0, slices + 1) ([0,1..] :: [Word16])
    withStorableArray idxs
      (\ptr -> GL.bufferData GL.ElementArrayBuffer GL.$= (iptrsize, ptr, GL.StaticDraw))

    return $ Shape (toEnum $ slices + 2) vbuf ibuf

vertShader :: [String]
vertShader = [
  "attribute vec2 position;",
  "uniform vec2 offset;",
  "void main() {",
  "  gl_Position = vec4(offset + 0.1*position, 1.0, 1.0);",
  "}"]

fragShader :: [String]
fragShader = [
  "uniform vec3 color;",
  "void main() {",
  "  gl_FragColor = vec4(color, 1.0);",
  "}"]

compileShader :: GL.ShaderType -> [String] -> IO (GL.Shader)
compileShader shdrTy src = do
  shdr <- GL.createShader shdrTy
  GL.shaderSource shdr GL.$= src
  GL.compileShader shdr
  shaderLog <- GL.get $ GL.shaderInfoLog shdr
  unless (shaderLog == []) $ putStrLn shaderLog
  return shdr

mkRenderFunc :: IO (Shape -> GL.Vertex2 Float -> GL.Color3 Float -> IO ())
mkRenderFunc = do
  -- Compile shaders
  vshdr <- compileShader GL.VertexShader vertShader
  fshdr <- compileShader GL.FragmentShader fragShader

  -- Create program
  prg <- GL.createProgram
  mapM_ (GL.attachShader prg) [vshdr, fshdr]
  GL.linkProgram prg

  -- Find variable locations
  colorLoc <- GL.get $ GL.uniformLocation prg "color"
  offsetLoc <- GL.get $ GL.uniformLocation prg "offset"
  positionLoc <- GL.get $ GL.attribLocation prg "position"

  GL.currentProgram GL.$= Just prg

  -- Define function
  return $ \(Shape numIdxs vbo ibo) pos color -> do

    -- Enable buffers
    GL.vertexAttribArray positionLoc GL.$= GL.Enabled

    -- Set uniforms
    GL.uniform colorLoc GL.$= ((fmap realToFrac color) :: GL.Color3 GL.GLfloat)
    GL.uniform offsetLoc GL.$= ((fmap realToFrac pos) :: GL.Vertex2 GL.GLfloat)

    -- Bind buffers
    GL.bindBuffer GL.ArrayBuffer GL.$= Just vbo
    GL.vertexAttribPointer positionLoc GL.$=
      (GL.ToFloat, GL.VertexArrayDescriptor 2 GL.Float 0 (nullPtr :: Ptr Float))

    GL.bindBuffer GL.ElementArrayBuffer GL.$= Just ibo

    -- Render
    GL.drawElements GL.TriangleFan numIdxs GL.UnsignedShort nullPtr

    -- Disable buffers
    GL.vertexAttribArray positionLoc GL.$= GL.Disabled    

--------------------------------------------------------------------------------

type RenderFn = GL.Vertex2 Float -> GL.Color3 Float -> IO ()
type GameMonad = GLFWInputT IO
type GameSession = Session IO (Timed Float ())

posWire :: Monoid e => Wire s e GameMonad a (GL.Vertex2 Float)
posWire = mouseCursor >>> (second $ arr negate) >>> (arr $ uncurry GL.Vertex2)

colorWire :: (HasTime t s, Monoid e) => Wire s e GameMonad a (GL.Color3 Float)
colorWire =
  (keyPressed GLFW.Key'R >>> (pure $ GL.Color3 1 0 0)) <|>
  (keyPressed GLFW.Key'G >>> (pure $ GL.Color3 0 1 0)) <|>
  (keyPressed GLFW.Key'B >>> (pure $ GL.Color3 0 0 1)) <|>
  (timeF >>> ((arr cos) &&& (arr sin)) >>> (arr $ \(x, y) -> GL.Color3 x y 1))

renderWire :: Monoid e => RenderFn -> Wire s e GameMonad (GL.Vertex2 Float, GL.Color3 Float) ()
renderWire rfn = mkGen_ $ \(pos, color) -> lift $ rfn pos color >> (return $ Right ())

-- Wire that behaves like the identity wire until Q is pressed, then inhibits forever.
quitWire :: Monoid e => Wire s e GameMonad a a
quitWire = (mkId &&& eventWire) >>> (rSwitch mkId)
  where
    eventWire :: Monoid e => Wire s e GameMonad a (Event (Wire s e m a a))
    eventWire = (keyPressed GLFW.Key'Q >>> pure mkEmpty >>> now) <|> never

gameWire :: (HasTime t s, Monoid e) => RenderFn -> Wire s e GameMonad a ()
gameWire rfn = posWire &&& colorWire >>> (renderWire rfn) >>> quitWire

run :: GLFW.Window -> GLFWInputControl -> IO ()
run win ictl = do
  ipt <- getInput ictl
  circle <- mkCircle
  renderFn <- mkRenderFunc
  runGame ipt (countSession_ 0.02) (gameWire $ renderFn circle)
  where
    runGame ipt sess w = do
      GL.clearColor GL.$= GL.Color4 0.0 0.0 0.0 1
      GL.clear [GL.ColorBuffer]

      (timeState, sess') <- stepSession sess
      let renderPrg = runStateT (stepWire w timeState (Right undefined)) ipt
      ((result, w'), ipt') <- renderPrg
      ipt'' <- pollGLFW ipt' ictl

      GL.flush
      GLFW.swapBuffers win

      case result of
        Left () -> return ()
        Right () -> do
          q <- GLFW.windowShouldClose win
          unless q $ runGame ipt'' sess' w'

main :: IO ()
main = do
  -- Setup GLFW
  GLFW.init
  (Just m) <- GLFW.createWindow 400 400 "Netwire Input Demo" Nothing Nothing
  GLFW.makeContextCurrent (Just m)

  -- Hack for retina displays
  (szx, szy) <- GLFW.getFramebufferSize m
  GL.viewport GL.$= (GL.Position 0 0, GL.Size (fromIntegral szx) (fromIntegral szy))

  -- Run the scene
  run m =<< (mkInputControl m)
