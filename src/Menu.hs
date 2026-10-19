module Menu
  ( menuButtons
  , hitTest
  ) where

import GameData (LevelId (..))

menuButtons :: [(LevelId, Float)]
menuButtons =
  [ (Sandbox,  150)
  , (Level1,    50)
  , (Level2,   -50)
  , (Level3,  -150)
  ]

--Remove hardcoded variables!!
hitTest :: (Float, Float) -> Maybe LevelId
hitTest (mx, my) = go menuButtons
  where
    go [] = Nothing
    go ((lid, cy) : rest)
      | abs mx <= 400 / 2, abs (my - cy) <= 70 / 2 = Just lid
      | otherwise = go rest
