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



--------------------------------------------------------------------------------------------------------------------------------------------
-- GHC Directives
--------------------------------------------------------------------------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Viewer.Shader where



-------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------------------------------------------------------------------------
import           Data.Maybe (fromMaybe, listToMaybe, isJust)

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW        as Context
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))
import qualified Graphics.UI.GLFW                   as GLFW

import Control.Lens

import Leibniz (deg, rad, π)

import Viewer.Types



-------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------------------------------------------------------------------

-- |
type UniformAt os f p a u = Shader os f (ShaderData os p) (UniformFormat (u) a)

uMatrix :: Int -> UniformAt os f a p (M44 (B Float))
uMatrix i = getUniform $ \sh -> (sh^.uniforms.matrices, i)

uScalar :: Int -> UniformAt os f a p (B Float)
uScalar i = getUniform $ \sh -> (sh^.uniforms.scalars, i)

uVector :: Int -> UniformAt os f a p (B3 Float)
uVector i = getUniform $ \sh -> (sh^.uniforms.vectors, i)


-- |
-- TODO: Rename
new :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) (FragmentStream (ColorSample F RGBFloat, FFloat))
new = do
  primitiveStream <- toPrimitiveStream (^.primitiveArray)
  
  -- TODO: Structured way of dealing with uniforms
  [mv, pv, tr] <- mapM uMatrix [0,1,2]
  
  -- Light position
  let light = V3 0 8 0

  let primitiveStream2 = fmap (\(p, n, t, c) -> (pv !*! (mv !*! tr) !* p, (n, t, c))) primitiveStream

  fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

  samp <- newSampler2D (\sh -> (sh^.texture._, sh^._.filterMode, (pure ClampToEdge, 0)))

  let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
      fragmentStream3 = fmap (\((n, p, c), d) -> (sampleTexture p + c, d)) fragmentStream2
  return fragmentStream3

-- ((V3 FFloat, V2 (S F Float), ColorSample F c0), FFloat)
-- ((V3 FFloat, V2 FFloat,      V3 FFloat),        FFloat)

-- |
-- TODO: Use a different environment (cf. 'mapShader' and 'maybeShader')
contour :: FragmentStream (ColorSample F RGBFloat, FFloat) -> Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
contour fragmentStream = do
  -- draw :: forall a os f s. (s -> Blending) -> FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
  -- TODO: Do we need to perform depth-testing (newShader should've taken care of that already)
  -- draw (const NoBlending) fragmentStream $ \c -> do
  colour <- uVector 0 -- Identifying silhouette colour (meshName)
  drawDepth (\sh -> (NoBlending, sh^.selection.depthTexture, DepthOption Less True)) fragmentStream $ \c -> do
    drawColor (\sh -> (sh^.selection.silhouettes, pure True, False)) (colour)


-- |
context :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
context = do
  fragmentStream <- new
  maybeShader (\sh -> if isJust (sh^.selection.meshName) then Just sh else Nothing) (contour fragmentStream)
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


-- |
interface :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
interface = do
  fragmentStream <- new
  drawContextColor (const (ContextColorOption NoBlending (pure True))) (fmap fst fragmentStream)