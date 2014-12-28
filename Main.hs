{-# LANGUAGE RankNTypes#-}
module Main where
import Control.Wire hiding (id)
import Data.Pong
import Foreign.C (CDouble(..))

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW (ErrorCallback, KeyCallback, Window)
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Graphics.UI.GLFW (getKey)

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
  render w clockSession_ $ createNetwork (getKey w)
  GLFW.destroyWindow w



render :: Window -> Session IO s -> Wire s e IO a GameInfo -> IO ()
render w session wire = do 

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
  (s, sess') <- stepSession session
  (stats, wire') <- stepWire wire s $ Right undefined
  case stats of
    Right (rotation, (a , b)) -> do 

                   GL.translate (Vector3 (CDouble a) (CDouble b) 0)
                   GL.rotate (CDouble rotation) (Vector3 0 0 1)
                   renderPrimitive 
                      Triangles 
                        $ do
                          GL.currentColor $= (GL.Color4 0 1 0 0)
                          GL.vertex (GL.Vertex3 (0.4 :: CDouble) (0.0) 0)
                          GL.currentColor $= (GL.Color4 0 0 1 0)
                          GL.vertex (GL.Vertex3 (0.2 :: CDouble) (0.2) 0)
                          GL.currentColor $= (GL.Color4 1 0 0 0)
                          GL.vertex (GL.Vertex3 (0.0 :: CDouble) (0.0) 0)

                   GLFW.swapBuffers w
                   GLFW.pollEvents
                              
                   render w sess' wire'
    _ -> return ()


handleInput :: KeyCallback
handleInput = undefined

sizeViewPort :: Window -> Int -> Int -> IO ()
sizeViewPort _ bx by = viewport $= (GL.Position 0 0, GL.Size (fromIntegral bx) (fromIntegral by))

printErrors :: ErrorCallback
printErrors e s = putStrLn $ show e ++ " : " ++ s
