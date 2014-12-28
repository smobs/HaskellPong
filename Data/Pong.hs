{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Data.Pong (
createNetwork,
GameInfo
) where

import Prelude hiding ((.))
import Control.Wire hiding (id)
import FRP.Netwire hiding (id)
import Graphics.UI.GLFW (getKey, KeyState(..))
import Graphics.UI.GLFW (Window)
import Graphics.UI.GLFW (Key(..))




type Speed = Double


type GameInfo = (Double, (Speed, Speed))

rotationAcceleration :: (forall a . Key -> Wire s () IO a ()) -> Wire s () IO c Speed
rotationAcceleration isDown = pure (0.0) . isDown (Key'A) . isDown (Key'S)
      <|> pure (-50) . isDown (Key'P) 
      <|> pure (50) . isDown (Key'O)
      <|> pure (0.0)

rotationSpeed :: HasTime t s => (forall b . Key -> Wire s () IO b ()) -> Wire s () IO a Speed
rotationSpeed isDown = integralWith stopFunction (0.0) 
               . zipApplicative  
                     (rotationAcceleration isDown) 
                     (isInhibiting $ isDown Key'L)

zipApplicative :: Applicative a => a r -> a t -> a (r, t) 
zipApplicative x y = (,) <$> x <*> y 

stopFunction :: Num a => Bool -> a -> a
stopFunction False = id
stopFunction True = const 0  

rotat :: HasTime t s => (forall b . Key -> Wire s () IO b ()) -> Wire s () IO a Speed
rotat isDown = integral (0.0) . (rotationSpeed isDown)

speed :: (Monad m, Monoid e) =>  (forall b . Key -> Wire s e m b ()) -> Wire s e m a (Speed, Speed)
speed isDown = 
    (verticalControlWire isDown (pure 0.0))
     &&&
     (horizontalControlWire isDown (pure 0.0))

verticalControlWire ::  (Monad m, Monoid e) =>  (forall b . Key -> Wire s e m b ()) -> Wire s e m a Speed ->  Wire s e m a Speed
verticalControlWire isDown = (<*>) (pure ((+) 0.5) . isDown Key'D
                              <|> pure ((+) (-0.5)) . isDown Key'A
                              <|> pure id)

horizontalControlWire ::  (Monad m, Monoid e)  =>  (forall b . Key -> Wire s e m b ()) -> Wire s e m a Speed -> Wire s e m a Speed
horizontalControlWire isDown =  (<*>) (pure ((+) 0.5) . isDown Key'W
                              <|> pure ((+) (-0.5)) . isDown Key'S
                              <|> pure id)

location :: (HasTime t s, Monad m, Monoid e) =>  (forall b . Key -> Wire s e m b ()) -> Wire s e m a (Double, Double)
location isDown = 
    integralPair (0.0, 0.0) . speed isDown

createNetwork  :: HasTime t s => Window -> Wire s () IO a (Double, (Speed, Speed))
createNetwork w = (rotat $ isKeyDown w) &&& (location $ isKeyDown w)

isInhibiting :: (Monoid e, Monad m) => Wire s e m a b -> Wire s e m a Bool
isInhibiting w = pure True . w
                 <|> pure False

integralPair :: (Fractional a, Fractional  b, HasTime t s, Monad m) => (a, b) -> Wire s e m (a, b) (a, b) 
integralPair (x, y) = (integral x) *** (integral y)


isKeyDown :: Window -> Key -> Wire s () IO a () 
isKeyDown w k = mkGen_ $ \_ -> do
                s <- getKey w k
                return $ case s of
                          KeyState'Pressed -> Right mempty
                          KeyState'Released -> Left mempty
                          KeyState'Repeating -> Right mempty
