{-# LANGUAGE RankNTypes#-}
module Main where
import qualified Graphics.UI.GLFW as GLFW
import Prelude hiding ((.))
import Control.Wire 
import Foreign.C (CDouble(..))

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW (ErrorCallback, Key(..), KeyCallback, Window)
import FRP.Netwire
import Graphics.UI.GLFW (KeyState(..))
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
  render w clockSession_ $ createNetwork w
  GLFW.destroyWindow w


render :: Window -> Session IO s -> Wire s e IO a Double -> IO ()
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
  (erotation, wire') <- stepWire wire s $ Right undefined
  case erotation of
    Right rotation -> do 
                   GL.rotate (CDouble rotation) (Vector3 0 0 1)
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
                              
                   render w sess' wire'
    Left _ -> return ()


handleInput :: KeyCallback
handleInput = undefined

sizeViewPort :: Window -> Int -> Int -> IO ()
sizeViewPort _ bx by = viewport $= (GL.Position 0 0, GL.Size (fromIntegral bx) (fromIntegral by))

printErrors :: ErrorCallback
printErrors e s = putStrLn $ show e ++ " : " ++ s

type Speed = Double

acceleration :: (forall a . Key -> Wire s () IO a ()) -> Wire s () IO c Speed
acceleration isDown = pure (0.0) . isDown (Key'A) . isDown (Key'S)
      <|> pure (-10) . isDown (Key'A) 
      <|> pure (10) . isDown (Key'S)
      <|> pure (0.0)

speed :: HasTime t s => (forall b . Key -> Wire s () IO b ()) -> Wire s () IO a Speed
speed isDown = integral (0.0) . (acceleration isDown)

rotat :: HasTime t s => (forall b . Key -> Wire s () IO b ()) -> Wire s () IO a Speed
rotat isDown = integral (0.0) . (speed isDown)

createNetwork  :: HasTime t s => Window -> Wire s () IO a Double
createNetwork w = rotat $ isKeyDown w

isKeyDown :: Window ->Key -> Wire s () IO a () 
isKeyDown w k = mkGen_ $ \_ -> do
                s <- getKey w k
                return $ case s of
                          KeyState'Pressed -> Right mempty
                          KeyState'Released -> Left mempty
                          KeyState'Repeating -> Right mempty
