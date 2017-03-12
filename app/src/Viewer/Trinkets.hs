-- |
-- Module      : Viewer.Trinkets
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
module Viewer.Trinkets where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Linear hiding (perspective)

import Leibniz (deg, rad, π)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- | Monadic no-op
pass :: Monad m => m ()
pass = return ()


-- | Are 'a' and 'b' within 'ε' of each other?
around :: (Num f, Ord f) => f -> f -> f -> Bool
around ε a b = abs (a-b) <= ε


-- | Safely behead a list
safeHead :: [a] -> Either String a
safeHead (x:_) = Right x
safeHead ([])  = Left "safeHead: empty list"


-- | Drops the Z coordinate
to2D :: V3 a -> V2 a
to2D (V3 x y _) = V2 x y


-- | Adds the Z coordinate
to3D :: a -> V2 a -> V3 a
to3D z (V2 x y) = V3 x y z


-- | Adds the W coordinate
to4D :: a -> V3 a -> V4 a
to4D w (V3 x y z) = V4 x y z w


-- | Creates a rotation matrix from an axis and an angle (in radians)
axisRotation :: (Floating f, Epsilon f) => V3 f -> f -> M33 f
axisRotation axis θ = fromQuaternion $ axisAngle axis θ


-- | Finds the right texture coordinate for a vertex
texcoord :: Fractional f => V3 f -> V2 f
texcoord = fmap ((*0.5) . (+1) . signum) . to2D
