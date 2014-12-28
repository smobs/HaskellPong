{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Data.Pong (
createNetwork,
GameInfo
) where

import Prelude hiding ((.))
import Control.Wire hiding (id)
import FRP.Netwire hiding (id)
import Graphics.UI.GLFW (KeyState(..))
import Graphics.UI.GLFW (Key(..))




type Speed = Double


type GameInfo = (Double, (Speed, Speed))


createNetwork  :: (Monad m, HasTime t s) => (Key -> m KeyState) -> Wire s () m a (Double, (Speed, Speed))
createNetwork isDown = (rotat $ isKeyDown isDown) &&& (location $ isKeyDown isDown)

rotationAcceleration :: Monad m => (forall a . Key -> Wire s () m a ()) -> Wire s () m c Speed
rotationAcceleration isDown = pure (0.0) . isDown (Key'A) . isDown (Key'S)
      <|> pure (-50) . isDown (Key'P) 
      <|> pure (50) . isDown (Key'O)
      <|> pure (0.0)

rotationSpeed :: (Monad m, HasTime t s) => (forall b . Key -> Wire s () m b ()) -> Wire s () m a Speed
rotationSpeed isDown = integralWith stopFunction (0.0) 
               . zipApplicative  
                     (rotationAcceleration isDown) 
                     (isInhibiting $ isDown Key'L)

zipApplicative :: Applicative a => a r -> a t -> a (r, t) 
zipApplicative x y = (,) <$> x <*> y 

stopFunction :: Num a => Bool -> a -> a
stopFunction False = id
stopFunction True = const 0  

rotat :: (Monad m, HasTime t s) => (forall b . Key -> Wire s () m b ()) -> Wire s () m a Speed
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


isInhibiting :: (Monoid e, Monad m) => Wire s e m a b -> Wire s e m a Bool
isInhibiting w = pure True . w
                 <|> pure False

integralPair :: (Fractional a, Fractional  b, HasTime t s, Monad m) => (a, b) -> Wire s e m (a, b) (a, b) 
integralPair (x, y) = (integral x) *** (integral y)


isKeyDown :: Monad m => (Key -> m KeyState) -> Key  -> Wire s () m a () 
isKeyDown keygen k = mkGen_ $ \_ -> do
                s <- keygen k
                return $ case s of
                          KeyState'Pressed -> Right mempty
                          KeyState'Released -> Left mempty
                          KeyState'Repeating -> Right mempty
