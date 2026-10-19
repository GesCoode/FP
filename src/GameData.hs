module GameData where

import Graphics.Gloss (Picture)

-- | 2D point / vector. Object positions use the center.
data Vec2 = Vec2 {x :: Float, y :: Float}
    deriving (Eq)

-- AABB: center + width + height.
data Hitbox = Hitbox {hbCenter :: Vec2, hbWidth :: Float, hbHeight :: Float}
    deriving (Eq)

-- Static level object (platform, block, …).
data StaticObj = StaticObj {soCenter :: Vec2, soWidth :: Float, soHeight :: Float, soSprite :: String}
    deriving (Eq)

-- Include direction
data PlayerStart = PlayerStart {psCenter :: Vec2, psVel :: Vec2}
    deriving (Eq)

data LevelId = Sandbox | Level1 | Level2 | Level3
  deriving (Eq)

levelName :: LevelId -> String
levelName Sandbox = "Sandbox"
levelName Level1  = "Level 1"
levelName Level2  = "Level 2"
levelName Level3  = "Level 3"

levelPath :: LevelId -> FilePath
levelPath Sandbox = "levels/sandbox.json"
levelPath Level1  = "levels/level1.json"
levelPath Level2  = "levels/level2.json"
levelPath Level3  = "levels/level3.json"

-- | Parsed level payload (level id comes from which file was loaded).
data LevelFile = LevelFile {lfStatics :: [StaticObj], lfPlayers :: [PlayerStart]}
    deriving (Eq)

data Level = Level { lvlId :: LevelId, lvlStatics :: [StaticObj], lvlPlayers :: [PlayerStart]}
    deriving (Eq)

data Player = Player { playerPos :: Vec2, playerVel :: Vec2, playerW :: Float, playerH :: Float}
    deriving (Eq)

-- Remove debug
data GameState = GameState {gsLevel :: Level, gsPlayer :: Player, gsDebug :: Bool}

data Screen = Menu | Playing GameState

type SpriteAtlas = [(String, Picture)]

data AppState = AppState {screen :: Screen, sprites :: SpriteAtlas}

initialApp :: SpriteAtlas -> AppState
initialApp atlas = AppState Menu atlas

-- Redo this
mkGameState :: Level -> GameState
mkGameState lvl = GameState
    { gsLevel = lvl
  , gsPlayer = case lvlPlayers lvl of
      (p:_) -> Player (psCenter p) (psVel p) 32 32
      []    -> Player (Vec2 0 0) (Vec2 0 0) 32 32
  , gsDebug  = True
  }

staticHitbox :: StaticObj -> Hitbox
staticHitbox s = Hitbox (soCenter s) (soWidth s) (soHeight s)

playerHitbox :: Player -> Hitbox
playerHitbox p = Hitbox (playerPos p) (playerW p) (playerH p)

overlaps :: Hitbox -> Hitbox -> Bool
overlaps a b =
  abs (x (hbCenter a) - x (hbCenter b)) < (hbWidth a + hbWidth b) / 2 &&
  abs (y (hbCenter a) - y (hbCenter b)) < (hbHeight a + hbHeight b) / 2
