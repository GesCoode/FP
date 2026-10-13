module Controller
  ( handleEvent
  , stepApp
  ) where

import Graphics.Gloss.Interface.IO.Game
  ( Event (..)
  , Key (..)
  , KeyState (..)
  , MouseButton (..)
  , SpecialKey (..)
  )

import GameData
  ( AppState (..)
  , GameState (..)
  , Player (..)
  , Screen (..)
  , Vec2 (..)
  , initialApp
  , mkGameState
  )
import LevelLoad (loadLevel)
import Menu (hitTest)
import Physics (stepPlayer)

playerSpeed :: Float
playerSpeed = 200

handleEvent :: Event -> AppState -> IO AppState
handleEvent (EventKey (MouseButton LeftButton) Down _ mouse) app =
  case screen app of
    Menu -> case hitTest mouse of
      Nothing  -> pure app
      Just lid -> do
        lvl <- loadLevel lid
        pure app { screen = Playing (mkGameState lvl) }
    Playing _ -> pure app

handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) app =
  case screen app of
    Playing _ -> pure (initialApp (sprites app))
    Menu      -> pure app

handleEvent (EventKey (Char 'd') Down _ _) app =
  pure $ mapPlaying app $ \gs -> gs { gsDebug = not (gsDebug gs) }
handleEvent (EventKey (Char 'D') Down _ _) app =
  pure $ mapPlaying app $ \gs -> gs { gsDebug = not (gsDebug gs) }

handleEvent (EventKey key keyState _ _) app =
  case keyState of
    Down -> pure $ mapPlaying app (applyKey key True)
    Up   -> pure $ mapPlaying app (applyKey key False)

handleEvent _ app = pure app

mapPlaying :: AppState -> (GameState -> GameState) -> AppState
mapPlaying app f = case screen app of
  Playing gs -> app { screen = Playing (f gs) }
  Menu       -> app

applyKey :: Key -> Bool -> GameState -> GameState
applyKey key pressed gs =
  let p = gsPlayer gs
      Vec2 vx0 vy0 = playerVel p
      (vx1, vy1) = case key of
        SpecialKey KeyLeft  -> (axis pressed (-playerSpeed) vx0, vy0)
        SpecialKey KeyRight -> (axis pressed   playerSpeed  vx0, vy0)
        SpecialKey KeyUp    -> (vx0, axis pressed   playerSpeed  vy0)
        SpecialKey KeyDown  -> (vx0, axis pressed (-playerSpeed) vy0)
        Char 'a'            -> (axis pressed (-playerSpeed) vx0, vy0)
        Char 'w'            -> (vx0, axis pressed   playerSpeed  vy0)
        Char 's'            -> (vx0, axis pressed (-playerSpeed) vy0)
        Char 'd'            -> (axis pressed (playerSpeed) vx0, vy0)
        --Char 'r'            -> (reset)
        _                   -> (vx0, vy0)
  in gs { gsPlayer = p { playerVel = Vec2 vx1 vy1 } }
  where
    axis True  speed _current = speed
    axis False speed current
      | abs (current - speed) < 1e-3 = 0
      | otherwise                    = current

stepApp :: Float -> AppState -> IO AppState
stepApp dt app = pure $ mapPlaying app (stepPlayer dt)
