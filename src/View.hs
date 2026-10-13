module View
  ( drawApp
  ) where

import qualified Data.Map.Strict as Map
import Graphics.Gloss

import GameData
  ( AppState (..)
  , GameState (..)
  , Hitbox (..)
  , Level (..)
  , LevelId (..)
  , Player (..)
  , Screen (..)
  , SpriteAtlas
  , StaticObj (..)
  , Vec2 (..)
  , levelName
  , playerHitbox
  )
import Menu (menuButtons)

grayButton :: Color
grayButton = makeColor 0.45 0.45 0.45 1.0

yellowButton :: Color
yellowButton = makeColor 1.0 0.85 0.0 1.0

drawApp :: AppState -> IO Picture
drawApp app = pure $ case screen app of
  Menu       -> drawMenu
  Playing gs -> drawPlaying (sprites app) gs

drawMenu :: Picture
drawMenu =
  pictures
    [ color black (rectangleSolid 800 600)
    , pictures (map drawButton menuButtons)
    , cornerLabel "Menu"
    ]

drawPlaying :: SpriteAtlas -> GameState -> Picture
drawPlaying atlas gs =
  pictures
    [ color black (rectangleSolid 800 600)
    , pictures (map (drawStatic atlas (gsDebug gs)) (lvlStatics (gsLevel gs)))
    , drawPlayer (gsDebug gs) (gsPlayer gs)
    , cornerLabel (levelName (lvlId (gsLevel gs)))
    ]

drawStatic :: SpriteAtlas -> Bool -> StaticObj -> Picture
drawStatic atlas debug s =
  translate (x c) (y c) $ pictures
    [ body
    , if debug
        then color green (rectangleWire (soWidth s) (soHeight s))
        else blank
    ]
  where
    c = soCenter s
    body = case Map.lookup (soSprite s) atlas of
      Just pic -> pic
      Nothing  -> color white $ rectangleSolid (soWidth s) (soHeight s)

drawPlayer :: Bool -> Player -> Picture
drawPlayer debug p =
  translate (x c) (y c) $ pictures
    [ color (makeColor 0.2 0.6 1.0 1.0) $
        rectangleSolid (playerW p) (playerH p)
    , if debug
        then color green $ rectangleWire (hbWidth hb) (hbHeight hb)
        else blank
    ]
  where
    c  = playerPos p
    hb = playerHitbox p

drawButton :: (LevelId, Float) -> Picture
drawButton (lid, cy) =
  translate 0 cy $ color (buttonColor lid) $
    rectangleSolid 400 70

buttonColor :: LevelId -> Color
buttonColor Sandbox = yellowButton
buttonColor _       = grayButton

cornerLabel :: String -> Picture
cornerLabel label =
  translate (-380) 270 $ scale 0.12 0.12 $ color white $ text label
