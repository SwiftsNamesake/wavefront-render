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
module Main where


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
  Paint     (s -> self -> Render ())          | -- ^ Can be rendered
  Transform (M44 f)                           | -- ^ Has a position, orientation and scaling
  Bounds    (BoundingBox (V3 f))              | -- ^ Has a bounding box
  Listen    (self -> ())                      | -- ^ Emits a sound
  Collide   (self -> self -> Maybe Collision) | -- ^ Can collide with another entity
  Select    MeshName                          | -- ^ Can be selected
  Skeleton  ()                                  -- ^ Has an animatable skeleton


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
  fComponents :: [EntityComponent os p]
}

-- fMesh      :: Mesh os p,
-- fBox       :: Mesh os Lines, -- TODO: Move this
-- fTransform :: M44 Float,
-- fTick      :: (Entity os p -> Float -> Entity os p),


-- |
data App os p = App {
  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B  Float)),
  fVectorUniforms :: Buffer os (Uniform (B3 Float)),
  
  fRasterOptions  :: (Side, ViewPort, DepthRange),
  fWindowSize     :: V2 Float,

  fMatrixValues   :: [M44 Float],
  fVectorValues   :: [V3 Float],
  fScalarValues   :: [Float],

  fSilhouettes  :: Texture2D os (Format RGBFloat),
  fDepthTexture :: Texture2D os (Format Depth),

  fEntities   :: [Entity os p],
  fShowBounds :: Bool,

  fInterface :: Interface os p
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

  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B  Float)),
  fVectorUniforms :: Buffer os (Uniform (B3 Float)),

  fTexture     :: Texture2D os (Format RGBFloat),
  fFilterMode  :: SamplerFilter RGBFloat,
  fEdgeMode    :: (EdgeMode2), --, BorderColor (Format RGBFloat)),


  fMeshName     :: Maybe MeshName,
  fSilhouettes  :: Image (Format RGBFloat),
  fDepthTexture :: Image (Format Depth)
}


makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Entity

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''Interface
makeLensesWith abbreviatedFields ''ShaderData


-- | Lens synonyms
perspectiveOf :: Simple Traversal (App os p) (M44 Float)
perspectiveOf = matrixValues.ix 1

modelviewOf :: Simple Traversal (App os p) (M44 Float)
modelviewOf = matrixValues.ix 0


