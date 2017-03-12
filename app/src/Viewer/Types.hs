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
-- module Viewer.Types (
--   fromMaybe, listToMaybe, isJust,

--   module Control.Lens,
--   module Graphics.GPipe,
--   module Cartesian.Core,
--   module Viewer.Types,
--   texture -- TODO: Why the fuck do I have to export this explicitly?
-- ) where
module Viewer.Types where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Set (Set)
import Data.Maybe (fromMaybe, listToMaybe, isJust)

import Control.Lens hiding (Level)

import Graphics.GPipe
import Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))

import Cartesian.Core (BoundingBox(..))



--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
type AppFormat = ContextFormat RGBFloat Depth
type AppT os a = ContextT GLFWWindow os AppFormat IO a

type MeshName = V3 Float -- TODO: Use integer instead (we'll need an integer texture)
type VertexAttribute = (B4 Float, B3 Float, B2 Float, B3 Float) -- TODO: Rename (eg. VertexAttributes)

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
--        - Use GADT or extensible record (cf. vinyl) (?)
--
data Components f os s self = Components {
  fAnimate   :: Maybe (Animate   f os s self), -- ^ Can be animated
  fPaint     :: Maybe (Paint     f os s self), -- ^ Can be rendered
  fTransform :: Maybe (Transform f os s self), -- ^ Has a position, orientation and scaling
  fBounds    :: Maybe (Bounds    f os s self), -- ^ Has a bounding box
  fListen    :: Maybe (Listen    f os s self), -- ^ Emits a sound
  fCollide   :: Maybe (Collide   f os s self), -- ^ Can collide with another entity
  fSelect    :: Maybe (Select    f os s self), -- ^ Can be selected
  fSkeleton  :: Maybe (Skeleton  f os s self), -- ^ Has an animatable skeleton
  fMortality :: Maybe (Mortality f os s self), -- ^ Has health
  fInventory :: Maybe (Inventory f os s self), -- ^ Has an inventory
  fLoot      :: Maybe (Loot      f os s self)  -- ^ Drops items when killed
}


newtype Animate   f os s self = Animate   (s -> self -> self)
newtype Paint     f os s self = Paint     (s -> self -> Render os s ())
newtype Transform f os s self = Transform (M44 f)
newtype Bounds    f os s self = Bounds    (BoundingBox (V3 f))
newtype Listen    f os s self = Listen    (self -> ())
newtype Collide   f os s self = Collide   (self -> self -> Maybe Collision)
newtype Select    f os s self = Select    MeshName
newtype Skeleton  f os s self = Skeleton  ()
newtype Mortality f os s self = Mortality f
newtype Inventory f os s self = Inventory ()
newtype Loot      f os s self = Loot      ()


-- |
-- type EntityComponent os p = Component Float (App os p) (Entity os p)


-- |
type Collision = ()

-- TODO: Use less rigid constructors (eg. GADTs)

-- |
data App os p = App {
  fRasterOptions :: (Side, ViewPort, DepthRange),
  fWindowSize    :: V2 Float,

  fShaders :: Shaders os p,

  fUniformBuffers :: UniformData os,
  fUniformValues  :: UniformValues Float,

  fSelection :: SelectionData,
  fEntities  :: [Entity os p],
  fInterface :: Interface os p,
  fSettings  :: AppSettings
}


-- |
data Shaders os p = Shaders {
  fSolid    :: CompiledShader os (ContextFormat RGBFloat Depth) (CommonShaderData os p),
  fWire     :: CompiledShader os (ContextFormat RGBFloat Depth) (CommonShaderData os Lines),
  fContours :: CompiledShader os (ContextFormat RGBFloat Depth) (CommonShaderData os p)
}


-- |
data AppSettings = AppSettings {
  fShowBounds :: Bool
}


-- |
data Mesh os p = Mesh {
  fVertices  :: Buffer os VertexAttribute,
  fPrimitive :: PrimitiveTopology p,
  fTexture   :: Texture2D os (Format RGBFloat)
}


-- |
data Entity os p = Entity {
  fComponents :: Components Float os (App os p) (Entity os p)
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
data CommonShaderData os p = CommonShaderData {
  fPrimitiveArray :: PrimitiveArray p VertexAttribute,
  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fUniforms :: UniformData os
}


-- |
data TextureData os = TextureData {
  fTexture     :: Texture2D os (Format RGBFloat),
  fFilterMode  :: SamplerFilter RGBFloat,
  fEdgeMode    :: (EdgeMode2) --, BorderColor (Format RGBFloat)),
  -- fBlend       :: (V3 Float -> V3 Float -> V3 Float) -- blend = (+)
}


-- |
data SelectionData = SelectionData {
  fName         :: Maybe MeshName,
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


-- | Auto-generated lenses (could we reduce this mess with some monadic incantation?)
makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''UniformValues
makeLensesWith abbreviatedFields ''Interface
makeLensesWith abbreviatedFields ''CommonShaderData
makeLensesWith abbreviatedFields ''Shaders
makeLensesWith abbreviatedFields ''AppSettings

makeLensesWith abbreviatedFields ''TextureData
makeLensesWith abbreviatedFields ''SelectionData
makeLensesWith abbreviatedFields ''ShadowData
makeLensesWith abbreviatedFields ''UniformData

makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Entity
makeLensesWith abbreviatedFields ''Components


-- | Lens synonyms
perspectiveOf :: Simple Traversal (App os p) (M44 Float)
perspectiveOf = uniformValues.matrices.ix 1

modelviewOf :: Simple Traversal (App os p) (M44 Float)
modelviewOf = uniformValues.matrices.ix 0
