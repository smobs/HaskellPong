{-# LANGUAGE RankNTypes, FlexibleContexts #-}
module Data.Pong where

import Prelude hiding ((.))
import Control.Wire
import FRP.Netwire
import Graphics.UI.GLFW (getKey, KeyState(..))
import Graphics.UI.GLFW (Window)
import Graphics.UI.GLFW (Key(..))

data Pong = MkPong { paddles :: [Paddle],
                     balls :: [Ball],
                     score :: PongScore}

data Paddle = Paddle
data Ball = Ball
newtype PongScore = MkScore { pongScores :: (Int, Int)}
newtype CollisionResult = MkColl {collisionResult :: Bool}

type Speed = Float
type Position = Speed
type IsDown m = (Monad m) => Key -> Wire s () m a ()

class HasExtent a where
    collides :: HasExtent b => a -> b -> CollisionResult

speed :: Monad m => IsDown m -> Wire s () m c Speed
speed isDown = pure (0.0) . isDown (Key'A) . isDown (Key'S)
      <|> pure (-0.5) . isDown (Key'A) 
      <|> pure (0.5) . isDown (Key'S)
      <|> pure (0.0)

pos :: (HasTime t s, Monad m) => IsDown m -> Wire s () m c Position
pos isDown = integral 0 . (speed isDown)

main :: IO ()
main = testWire clockSession_ $ pos (\k-> case k of
                                            Key'A -> pure ()
                                            _ -> mkEmpty
                                    ) 

isKeyDown :: Window -> IsDown IO
isKeyDown w k = mkGen_ $ \_ -> do
                s <- getKey w k
                return $ case s of
                          KeyState'Pressed -> Right mempty
                          KeyState'Released -> Left mempty
                          KeyState'Repeating -> Right mempty
