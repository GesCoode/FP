module Physics
  ( stepPlayer
  ) where

import GameData
  ( GameState (..)
  , Level (..)
  , Player (..)
  , Vec2 (..)
  , overlaps
  , playerHitbox
  , staticHitbox
  )

-- | Integrate velocity and resolve AABB overlaps with statics (separate axes).
stepPlayer :: Float -> GameState -> GameState
stepPlayer dt gs =
  let p0 = gsPlayer gs
      vx' = x (playerVel p0)
      vy' = y (playerVel p0)
      pX  = moveAxis dt (\v dx -> v { x = x v + dx }) vx' p0
      pY  = moveAxis dt (\v dy -> v { y = y v + dy }) vy' pX
  in gs { gsPlayer = pY }
  where
    solids = map staticHitbox (lvlStatics (gsLevel gs))

    moveAxis dtSec setCoord speed pl =
      let pos1 = setCoord (playerPos pl) (speed * dtSec)
          pl1  = pl { playerPos = pos1 }
      in if any (overlaps (playerHitbox pl1)) solids
           then pl  -- cancel this axis' movement
           else pl1
