module Main where

import Data.Map.Strict (fromList)
import Graphics.Gloss (Display (..), Picture, black, color, loadBMP, rectangleSolid, white)
import Graphics.Gloss.Interface.IO.Game (playIO)
import System.Directory (doesFileExist)

import Controller (handleEvent, stepApp)
import GameData (SpriteAtlas, initialApp)
import View (drawApp)

fps :: Int
fps = 60

loadSprite :: String -> FilePath -> IO (String, Picture)
loadSprite name path = do
  exists <- doesFileExist path
  pic <- if exists
    then loadBMP path
    else pure (color white (rectangleSolid 120 80))
  pure (name, pic)

loadSprites :: IO SpriteAtlas
loadSprites = do
  block <- loadSprite "block" "assets/block.bmp"
  pure (fromList [block])

main :: IO ()
main = do
  atlas <- loadSprites
  playIO
    (InWindow "MarIO" (800, 600) (100, 100))
    black
    fps
    (initialApp atlas)
    drawApp
    handleEvent
    stepApp
