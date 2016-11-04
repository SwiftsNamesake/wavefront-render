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
module Viewer.Shader where



-------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
-------------------------------------------------------------------------------------------------------------------------------------------
import Leibniz (deg, rad, π)



-------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
-------------------------------------------------------------------------------------------------------------------------------------------

-- |
type UniformAt os f p a u = Shader os f (ShaderData os p) (UniformFormat (u) a)

matrixUniform :: Int -> UniformAt os f a p (M44 (B Float))
matrixUniform i = getUniform $ \sh -> (sh^.matrixUniforms, i)

scalarUniform :: Int -> UniformAt os f a p (B Float)
scalarUniform i = getUniform $ \sh -> (sh^.scalarUniforms, i)

vectorUniform :: Int -> UniformAt os f a p (B3 Float)
vectorUniform i = getUniform $ \sh -> (sh^.vectorUniforms, i)


-- |
-- TODO: Rename
newShader :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) (FragmentStream (ColorSample F RGBFloat, FFloat))
newShader = do
  primitiveStream <- toPrimitiveStream (^.primitiveArray)

  -- TODO: Structured way of dealing with uniforms
  [mv, pv, tr] <- mapM matrixUniform [0,1,2]
  
  -- Light position
  let light = V3 0 8 0

  let primitiveStream2 = fmap (\(p, n, t, c) -> (pv !*! (mv !*! tr) !* p, (n, t, c))) primitiveStream

  fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

  samp <- newSampler2D (\sh -> (sh^.texture, sh^.filterMode, (pure ClampToEdge, 0)))

  let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
      fragmentStream3 = fmap (\((n, p, c), d) -> (sampleTexture p + c, d)) fragmentStream2
  return fragmentStream3


-- |
-- TODO: Use a different environment (cf. 'mapShader' and 'maybeShader')
contourShader :: FragmentStream (ColorSample F RGBFloat, FFloat) -> Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
contourShader fragmentStream = do
  -- draw :: forall a os f s. (s -> Blending) -> FragmentStream a -> (a -> DrawColors os s ()) -> Shader os f s ()
  -- TODO: Do we need to perform depth-testing (newShader should've taken care of that already)
  -- draw (const NoBlending) fragmentStream $ \c -> do
  colour <- vectorUniform 0 -- Identifying silhouette colour (meshName)
  drawDepth (\sh -> (NoBlending, sh^.depthTexture, DepthOption Less True)) fragmentStream $ \c -> do
    drawColor (\sh -> (sh^.silhouettes, pure True, False)) (colour)


-- | 
-- TODO: Rename (?)
-- TODO: Dealing with identifiers (simply use V3 colours?)
-- newCollisionShader :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
-- newCollisionShader = do
--   fragmentStream <- newShader


-- |
newContextShader :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
newContextShader = do
  fragmentStream <- newShader
  maybeShader (\sh -> if isJust (sh^.meshName) then Just sh else Nothing) (contourShader fragmentStream)
  drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream


-- |
newInterfaceShader :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
newInterfaceShader = do
  fragmentStream <- newShader
  drawContextColor (const (ContextColorOption NoBlending (pure True))) (fmap fst fragmentStream)