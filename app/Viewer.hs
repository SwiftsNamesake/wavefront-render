-- |
-- Module      : Viewer - Main
-- Description : Wavefront OBJ viewer
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
{-# LANGUAGE
      ScopedTypeVariables
    , PackageImports
    , TypeFamilies
    , FlexibleContexts
    , TemplateHaskell
    , MultiParamTypeClasses
    , FunctionalDependencies
    , FlexibleInstances
    , DuplicateRecordFields
#-}



--------------------------------------------------------------------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Main where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import           Data.Monoid
import           Data.Foldable (toList)
import           Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import           Data.Vector ((!?), (!), Vector)
import qualified Data.Text as T

import System.FilePath

import Text.Printf

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as Context
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow(..))
import qualified Graphics.UI.GLFW as GLFW

import qualified Codec.Picture       as Juicy
import qualified Codec.Picture.Types as Juicy
import Control.Applicative
import Control.Monad (unless)
import Control.Arrow (first)
import Control.Lens
import Control.Monad.IO.Class

import Linear hiding (perspective)

import Leibniz (π)

import           Graphics.WaveFront
import qualified Graphics.WaveFront.Load  as Load



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data App os f = App {
  fVertices       :: Buffer os (B4 Float, B2 Float),
  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B Float)),

  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fMatrixValues :: [M44 (Float)],
  fScalarValues :: [Float]

}
-- fShaderOf       :: CompiledShader os f (App os f)


-- |
data ShaderData = ShaderData {
  fPrimitiveArray :: PrimitiveArray Triangles (B4 Float, B2 Float),
  fRasterOptions  :: (Side, ViewPort, DepthRange)
}


makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''ShaderData


-- |
-- instance Epsilon (S V Float) where
--   nearZero = (<=* 1e-6) . abs



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

plane :: Fractional f => (f -> f -> a) -> f -> f -> [a]
plane f da db = [f (-da/2) (db/2), f (da/2) (db/2), f (da/2) (-db/2), f (-da/2) (-db/2)]
-- plane f dx dy dz = [f (-dx/2) (dy/2) (dz/2), f (dx/2) (dy/2) (dz/2), f (dx/2) (-dy/2) (-dz/2), f (-dx/2) (-dy/2) (-dz/2)]

-- TODO: Deal with clock-wise/counter-clockwise issues (if there are any)

-- | Generate the vertices for a rectangular plane, parallel with the X and Y axes and centred at (0,0,0)
planeXY :: Fractional f => (f -> f -> f -> a) -> f -> f -> [a]
planeXY f dx dy = triangles $ plane (\x y -> f x y 0) dx dy


triangles :: [a] -> [a]
triangles (a:rest) = concat $ pairwise (\b c -> [a, b, c]) rest
  where
    -- | Combine every adjacent pair in the list with the given function
    pairwise :: (a -> a -> b) -> [a] -> [b]
    pairwise f xs = zipWith f xs (drop 1 xs)


--------------------------------------------------------------------------------------------------------------------------------------------

-- | Build a matrix for a symmetric perspective-view frustum
-- Borrowed from Linear.Perspective
-- perspective
--   :: Floating a
--   => a -- ^ FOV
--   -> a -- ^ Aspect ratio
--   -> a -- ^ Near plane
--   -> a -- ^ Far plane
--   -> M44 a
-- perspective fovy aspect near far =
--   V4 (V4 x 0 0    0)
--      (V4 0 y 0    0)
--      (V4 0 0 z    w)
--      (V4 0 0 (-1) 0)
--   where tanHalfFovy = tan $ fovy / 2
--         x = 1 / (aspect * tanHalfFovy)
--         y = 1 / tanHalfFovy
--         z = -(far + near) / (far - near)
--         w = -(2 * far * near) / (far - near)


-- |
-- matrix
-- matrix d = (d^.modelview) !*! (d^.projection)


-- |
-- rotationMatrix = m33_to_m44 . fromQuaternion . axisAngle (V3 0 0 1)
rotation :: S V Float -> M44 (S V Float)
rotation a = V4 (V4 (cos a) (-sin a) 0 0)
                (V4 (sin a) ( cos a) 0 0)
                (V4      0        0  1 0)
                (V4      0        0  0 1)

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Load a 'TexInfo' value from an image file, and supply it to a
-- user-provided function. Supported image formats include @png@,
-- @jpeg@, @bmp@, and @gif@. See 'readTexture' for most uses.
-- readTexInfo :: FilePath -> (forall a. IsPixelData a => TexInfo a -> IO b) -> IO (Either String b)
-- readTexInfo f k = readImage f >>= either (return . Left) aux
--   where
--     aux (Juicy.ImageY8 (Juicy.Image w h p))    = Right <$> k (texInfo w h Juicy.TexMono p)
--     aux (Juicy.ImageYF (Juicy.Image w h p))    = Right <$> k (texInfo w h Juicy.TexMono p)
--     aux (Juicy.ImageYA8 _)               = return $ Left "YA format not supported"
--     aux (Juicy.ImageRGB8 (Juicy.Image w h p))  = Right <$> k (texInfo w h Juicy.TexRGB p)
--     aux (Juicy.ImageRGBF (Juicy.Image w h p))  = Right <$> k (texInfo w h Juicy.TexRGB p)
--     aux (Juicy.ImageRGBA8 (Juicy.Image w h p)) = Right <$> k (texInfo w h Juicy.TexRGBA p)
--     aux (Juicy.ImageYCbCr8 img) = aux . Juicy.ImageRGB8 $ convertImage img
--     aux _ = return $ Left "Unsupported image format"

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
getJuicyPixel xs _x _y pix = let Juicy.PixelRGB8 r g b = Juicy.convertPixel (pix) in V3 r g b : xs


-- |
loadTexture :: MonadIO m => FilePath -> ContextT w os f m (Either a (V2 Int, Texture2D os (Format RGBFloat)))
loadTexture fn = do
  liftIO (putStrLn fn)
  (Right (Juicy.ImageRGB8 image')) <- liftIO $ Juicy.readImage fn
  let size' = V2 (Juicy.imageWidth image') (Juicy.imageHeight image')
  tex <- newTexture2D SRGB8 size' maxBound -- JPG converts to SRGB
  writeTexture2D tex 0 0 size' $ Juicy.pixelFold getJuicyPixel [] image'
  generateTexture2DMipmap tex
  return (Right (size', tex))

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do
  -- Load all the things
  let root = "C:/Users/Jonatan/Desktop/Haskell/modules/wavefront-render"

  (Right m) <- Load.model "assets/models/minecraft1.obj"

  let vIndex coords i  = coords !? (i-1)
      tIndex coords mi = fromMaybe (V2 0 0) $ mi >>= \i -> (coords !? (i-1))
      (Just vs) = sequence $ fromFaceIndices (m^.vertices)  (vIndex) (^.ivertex)   (m^.faces)
      ts        =            fromFaceIndices (m^.texcoords) (tIndex) (^.itexcoord) (m^.faces)
      cs        = diffuseColours (m^.faces)

  printf "Done loading model with %d vertices.\n" (length vs)

  let box = bounds m
  print box

  -- Render
  runContextT Context.newContext (ContextFormatColorDepth RGB8 Depth24) $ do
    -- Create texture
    (Right (size', tex)) <- loadTexture $ root </> "assets/models/textures/" </> (T.unpack . head . toList $ textures m)
    -- let (V2 tx ty) = fmap ((/512) . fromIntegral) size'

    vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer (length vs)
    writeBuffer vertexBuffer 0 $ zipWith (\(V3 x y z) (V2 tx ty) -> (V4 x y z 1, V2 (1-tx) ty)) (toList vs) (toList ts)
    -- vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer (length $ planeXY V3 2 2)
    -- writeBuffer vertexBuffer 0 $ zipWith (\(V3 x y z) tex -> (V4 x y z 1, tex)) (planeXY V3 tx ty) (planeXY (\x y _ -> pure 0.5 + V2 x y) 1 1)

    scalars  :: Buffer os (Uniform (B Float))       <- newBuffer 3
    matrices :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 2

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream (^.primitiveArray)
      
      [mv, pv] <- mapM (\i -> getUniform $ const (matrices, i)) [0,1]
      -- [scale]  <- mapM (\i -> getUniform $ const (scalars,  i)) [0]

      let primitiveStream2 = fmap (\(p, t) -> (pv !*! mv !* p, t)) primitiveStream
      -- let primitiveStream2 = fmap (\(p, t) -> (mv !* p, t)) primitiveStream

      fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

      let filterMode = SamplerFilter Linear Linear Linear (Just 4)
          edgeMode   = (pure ClampToEdge, undefined)
      samp <- newSampler2D (const (tex, filterMode, edgeMode))
      let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
          fragmentStream3 = fmap (\(p, d) -> (sampleTexture p, d)) fragmentStream2

      drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream3

    loop shader $ App { fVertices = vertexBuffer,
                        fScalarUniforms = scalars,
                        fMatrixUniforms = matrices,

                        fScalarValues = [0,0,0],
                        fMatrixValues = [mkTransformationMat (fromQuaternion $ axisAngle (V3 1 0 0) (0)) (V3 0 0 0),
                                         perspective (60 * π/180) (1) 1 1000] }


-- |
-- tick :: 
tick app' = do
  size@(V2 cx' cy') <- uncurry V2 <$> withContextWindow (GLFW.getWindowSize . getGLFWWindow)
  mouse             <- uncurry V2 <$> Context.getCursorPos
  
  -- let (V3 x' y' z') = position mouse size
  let (V2 mx my)    = fmap realToFrac mouse
      (V2 cx cy)    = fmap fromIntegral size

  (Just t) <- fmap realToFrac <$> liftIO GLFW.getTime

  let app = app' & scalarValues.ix 0 .~ (5 * mx/my)
                 & matrixValues.ix 1 .~ perspective (60 * π/180) (cx/cy) 1 50
                 & matrixValues.ix 0 .~ mkTransformationMat (fromQuaternion $ axisAngle (V3 0 1 0) (t*0.2*360*π/180)) (position mouse size)
                 & rasterOptions     .~ (FrontAndBack, ViewPort (V2 0 0) size, DepthRange 0 50)
    
  writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues)
  writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)

  return app
  where
    position mouse size = let (V2 mx my) = fmap realToFrac mouse
                              (V2 cx cy) = fmap fromIntegral size in V3 0 (30*((-0.5) + mx/cx)) ((-30)*my/cy)


-- |
-- loop :: App os f -> IO ()
loop shader app' = do

  app <- tick app'
  
  render $ do
    clearContextColor (V3 0.78 0.78 0.78)
    clearContextDepth 1.0
    vertexArray <- newVertexArray (app^.vertices)
    (shader) $ ShaderData { fPrimitiveArray = toPrimitiveArray TriangleList vertexArray,
                            fRasterOptions  = app^.rasterOptions }
  swapContextBuffers

  closeRequested <- Context.windowShouldClose
  unless closeRequested $
    loop shader app
