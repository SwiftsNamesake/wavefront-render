-- |
-- Module      : Viewer.Entity
-- Description : 
-- Copyright   : (c) Jonatan H Sundqvist, 2016
-- License     : MIT
-- Maintainer  : Jonatan H Sundqvist
-- Stability   :
-- Portability :

-- TODO | -
--        -

-- SPEC | -
--        -



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Viewer.Mesh where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Linear hiding (perspective)
import Graphics.GPipe

import Leibniz (deg, rad, π)

import           Viewer.Types
import qualified Viewer.Shader as Shader
import qualified Viewer.Mesh   as Mesh
import           Viewer.Trinkets



--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | A new component set with no features
new :: Components f os s self
new = Components {
  fAnimate   = Nothing, -- ^ Can be animated
  fPaint     = Nothing, -- ^ Can be rendered
  fTransform = Nothing, -- ^ Has a position, orientation and scaling
  fBounds    = Nothing, -- ^ Has a bounding box
  fListen    = Nothing, -- ^ Emits a sound
  fCollide   = Nothing, -- ^ Can collide with another entity
  fSelect    = Nothing, -- ^ Can be selected
  fSkeleton  = Nothing, -- ^ Has an animatable skeleton
  fMortality = Nothing, -- ^ Has health
  fInventory = Nothing, -- ^ Has an inventory
  fLoot      = Nothing  -- ^ Drops items when killed
}
