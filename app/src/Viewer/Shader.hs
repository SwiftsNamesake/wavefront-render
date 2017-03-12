-- |
-- Module      : Viewer.Shader
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



-- GHC Directives --------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE DuplicateRecordFields  #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Viewer.Shader where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import Data.Maybe (isJust)
import Linear hiding (perspective, texture)
import Control.Lens hiding (texture)

import           Graphics.GPipe hiding (texture)
import qualified Graphics.GPipe.Context.GLFW as Context hiding (texture)

import Cartesian.Core (z)

import Leibniz (deg, rad, π)

import Viewer.Types
import Viewer.Trinkets

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- TODO: Wrap in maybe (?)

uMatrix :: Int -> Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) (UniformFormat (M44 (B Float)) a)
uMatrix i = getUniform $ \sh -> (sh^.uniforms.matrices, i)

uScalar :: Int -> Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) (UniformFormat (B Float) a)
uScalar i = getUniform $ \sh -> (sh^.uniforms.scalars, i)

uVector :: Int -> Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) (UniformFormat (B3 Float) a)
uVector i = getUniform $ \sh -> (sh^.uniforms.vectors, i)

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename
new :: Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) (FragmentStream (ColorSample F RGBFloat, FFloat))
new = do
  -- One day, in the distant future, I will come to know what a primitive stream is
  primitiveStream <- toPrimitiveStream (^.primitiveArray)

  -- TODO: Structured way of dealing with uniforms
  [mv, pv, tr] <- mapM uMatrix [0,1,2]

  -- Light position
  let light = V3 0 8 0

  let primitiveStream2 = fmap (\(p, n, t, c) -> (pv !*! (mv !*! tr) !* p, (n, t, c))) primitiveStream

  fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

  samp <- newSampler2D (\sh -> (sh^.texture.texture, sh^.texture.filterMode, (pure ClampToEdge, 0)))

  let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = withRasterizedInfo (\(a :: (V3 FFloat, V2 FFloat, V3 FFloat)) info -> (a, rasterizedFragCoord info^.z)) fragmentStream
      fragmentStream3 = fmap (\((n, p, c), d) -> (sampleTexture p + c :: Color RGBFloat _, d)) (fragmentStream2)
  return fragmentStream3


-- |
-- TODO: Use a different environment (cf. 'mapShader' and 'maybeShader')
contour :: FragmentStream (ColorSample F RGBFloat, FFloat) -> Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) ()
contour fragmentStream = do
  -- draw :: forall a os f s. (s -> Blending) -> FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
  -- TODO: Do we need to perform depth-testing (newShader should've taken care of that already)
  -- draw (const NoBlending) fragmentStream $ \c -> do
  -- colour <- uVector 0 -- Identifying silhouette colour (meshName)
  drawDepth (\sh -> (NoBlending, let (Just sel) = sh^.selection in sel^.depthTexture, DepthOption Less True)) fragmentStream $ \c -> do
    drawColor (\sh -> (sh^.selection.silhouettes, pure True, False)) c


-- |
context :: Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) ()
context = do
  fragmentStream <- new
  maybeShader (\sh -> if isJust (sh^.selection) then Just sh else Nothing) (contour fragmentStream)
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


-- |
interface :: Shader os (ContextFormat RGBFloat Depth) (CommonShaderData os p) ()
interface = do
  fragmentStream <- new
  drawContextColor (const (ContextColorOption NoBlending (pure True))) (fmap fst fragmentStream)

--------------------------------------------------------------------------------------------------------------------------------------------

-- | TODO: Constructing CommonShaderData based on Components

-- |
-- TODO | - Rename (?)
makeCommonShaderData :: App os p -> Mesh os p -> Render os t (CommonShaderData os p)
makeCommonShaderData app mesh = do
  vertexArray <- newVertexArray (mesh^.vertices)
  return $ CommonShaderData {
    fPrimitiveArray = toPrimitiveArray (mesh^.primitive) vertexArray,
    fUniforms       = app^.uniformBuffers,
    fRasterOptions  = app^.rasterOptions,
    fTexture   = Nothing,
    fSelection = Nothing }


-- |
makeTextureData :: Mesh os p -> TextureData os
makeTextureData mesh = TextureData { fFilterMode = SamplerFilter Linear Linear Linear (Just 4),
                                     fEdgeMode   = (pure ClampToEdge), --, pure 1),
                                     fTexture    = mesh^.texture }


-- |
makeSelectionData :: App os p -> MeshName -> SelectionData
makeSelectionData app nameOf = (app^.selection) & name .~ Just nameOf
