-- |
-- Module      : Viewer.Types
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
-- GHC Directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Viewer.Types (
  fromMaybe, listToMaybe, isJust,

  module Control.Lens,
  module Graphics.GPipe,
  module Cartesian.Core,
  module Viewer.Types
) where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Set (Set)
import Data.Maybe (fromMaybe, listToMaybe, isJust)

import Control.Lens hiding (Level)

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))

import Graphics.WaveFront

import Cartesian.Core (BoundingBox(..))


--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
type AppFormat = ContextFormat RGBFloat Depth
type AppT os a = ContextT GLFWWindow os AppFormat IO a

appFormat = ContextFormatColorDepth RGB8 Depth32

type MeshName = V3 Float -- TODO: Use integer instead (we'll need an integer texture)
type VertexAttribute = (B4 Float, B3 Float, B2 Float, B3 Float)

-- | An entity is but a bundle of components
-- TODO | - How to prevent clashes (eg. duplicate animation components) 
--        - Efficient lookups

--        - Private state
--          - Are closures enough (?)
--          - Wrap in monad (?)
--          - Limit access to state (?)

--        - Dependencies
--          - Co-dependent components
--          - Affecting the outside state (should that be allowed?)

--        - Naming convention
--        - Polymorphism (?)
--        - Use GADT (?)
data Component f s self =
  Animate   (s -> self -> self)               | -- ^ Can be animated
  -- Paint     (s -> self -> Render os s ())     | -- ^ Can be rendered
  Transform (M44 f)                           | -- ^ Has a position, orientation and scaling
  Bounds    (BoundingBox (V3 f))              | -- ^ Has a bounding box
  Listen    (self -> ())                      | -- ^ Emits a sound
  Collide   (self -> self -> Maybe Collision) | -- ^ Can collide with another entity
  Select    MeshName                          | -- ^ Can be selected
  Skeleton  ()                                | -- ^ Has an animatable skeleton
  Mortality f                                   -- ^ Has health


-- |
type EntityComponent os p = Component Float (App os p) (Entity os p)


-- |
type Collision = ()


-- TODO: Use less rigid constructors (eg. GADTs)

-- |
data Mesh os p = Mesh {
  fVertices  :: Buffer os VertexAttribute,
  fPrimitive :: PrimitiveTopology p,
  fTexture   :: Texture2D os (Format RGBFloat),
  fMeshName  :: Maybe MeshName
}


-- |
data Entity os p = Entity {
  fComponents :: Set (EntityComponent os p)
}

-- fMesh      :: Mesh os p,
-- fBox       :: Mesh os Lines, -- TODO: Move this
-- fTransform :: M44 Float,
-- fTick      :: (Entity os p -> Float -> Entity os p),


-- |
data App os p = App {
  fRasterOptions :: (Side, ViewPort, DepthRange),
  fWindowSize    :: V2 Float,

  fUniformBuffers :: UniformData os,
  fUniformValues  :: UniformValues Float,

  fSelection :: SelectionData,

  fEntities   :: [Entity os p],
  fShowBounds :: Bool,

  fInterface :: Interface os p
}


-- |
data UniformValues f = UniformValues {
  fMatrices :: [M44 f],
  fVectors  :: [V3 f],
  fScalars  :: [f]
}


-- | (B4 Float, B3 Float, B2 Float, B3 Float)
data Interface os p = Interface {
 fMinimap  :: Mesh os p,
 fMiniSize :: V2 Int
}


-- |
data ShaderData os p = ShaderData {
  fPrimitiveArray :: PrimitiveArray p VertexAttribute,
  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fSelection :: SelectionData,

  fUniforms :: UniformData os,
  fTexture  :: TextureData os
}


-- |
data TextureData os = TextureData {
  fTexture     :: Texture2D os (Format RGBFloat),
  fFilterMode  :: SamplerFilter RGBFloat,
  fEdgeMode    :: (EdgeMode2), --, BorderColor (Format RGBFloat)),
  fBlend       :: (V3 Float -> V3 Float -> V3 Float) -- blend = (+)
}


-- |
data SelectionData = SelectionData {
  fMeshName     :: Maybe MeshName,
  fSilhouettes  :: Image (Format RGBFloat),
  fDepthTexture :: Image (Format Depth)
}


-- | I love the word 'shadow'. Who knows why.
data ShadowData = ShadowData {
  
}


-- |
data UniformData os = UniformData {
  fMatrices :: Buffer os (Uniform (M44 (B Float))),
  fScalars  :: Buffer os (Uniform (B  Float)),
  fVectors  :: Buffer os (Uniform (B3 Float))
}


-- | Auto-generated lenses
makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Entity

makeLensesWith abbreviatedFields ''TextureData
makeLensesWith abbreviatedFields ''SelectionData
makeLensesWith abbreviatedFields ''ShadowData
makeLensesWith abbreviatedFields ''UniformData

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''UniformValues
makeLensesWith abbreviatedFields ''Interface
makeLensesWith abbreviatedFields ''ShaderData


-- | Lens synonyms
perspectiveOf :: Simple Traversal (App os p) (M44 Float)
perspectiveOf = uniformValues.matrices.ix 1

modelviewOf :: Simple Traversal (App os p) (M44 Float)
modelviewOf = uniformValues.matrices.ix 0


