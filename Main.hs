module Main where
import qualified Graphics.UI.GLFW as GLFW
 
import Graphics.UI.GLFW (ErrorCallback)
import Graphics.UI.GLFW (KeyCallback)
import Graphics.UI.GLFW (Window)

import Graphics.Rendering.OpenGL as GL
import Foreign.C (CDouble(..))
import Control.Monad (forever)

main :: IO ()
main = do
  True <- GLFW.init
  GLFW.defaultWindowHints
  GLFW.setErrorCallback $ Nothing
  Just window <- GLFW.createWindow 640 480 "Pong"  Nothing Nothing   
  GLFW.makeContextCurrent (Just window)
  windowWork window
  GLFW.terminate

windowWork :: Window -> IO ()
windowWork w = do
  GLFW.setWindowCloseCallback w (Just GLFW.destroyWindow)
  GLFW.setFramebufferSizeCallback w $ Just sizeViewPort
  _ <- forever $ render w
  GLFW.destroyWindow w

render :: Window -> IO ()
render w = do 

  (x, y) <-  GLFW.getFramebufferSize w
  let ratio = (fromIntegral x) / (fromIntegral y) :: GLdouble

  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral x) (fromIntegral y))
  GL.clearColor $= Color4 0 1 1 0
  GL.clear[GL.ColorBuffer]

  GL.matrixMode $= GL.Projection
  GL.loadIdentity

  GL.ortho (-ratio) ratio (-1) 1 1 (-1)

  GL.matrixMode $= GL.Modelview 0
  GL.loadIdentity
  time <- GLFW.getTime
  GL.rotate (CDouble (maybe 0 id time)* 50) (Vector3 0 0 1)

  renderPrimitive 
           Triangles 
            $ do
             GL.currentColor $= (GL.Color4 0 1 0 0)
             GL.vertex (GL.Vertex3 (-0.6 :: CDouble) (-0.4) 0)
             GL.currentColor $= (GL.Color4 0 0 1 0)
             GL.vertex (GL.Vertex3 (0.6 :: CDouble) (-0.4) 0)
             GL.currentColor $= (GL.Color4 1 0 0 0)
             GL.vertex (GL.Vertex3 (0.0 :: CDouble) (0.6) 0)

  GLFW.swapBuffers w
  GLFW.pollEvents


handleInput :: KeyCallback
handleInput = undefined

sizeViewPort :: Window -> Int -> Int -> IO ()
sizeViewPort _ bx by = viewport $= (GL.Position 0 0, GL.Size (fromIntegral bx) (fromIntegral by))

printErrors :: ErrorCallback
printErrors e s = putStrLn $ show e ++ " : " ++ s

