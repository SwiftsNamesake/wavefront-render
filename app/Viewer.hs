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
import           Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Vector as V
import           Data.Vector ((!?), (!), Vector)
import qualified Data.Text as T
import           Data.Text (Text)

import System.FilePath

import Text.Printf

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW as Context
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow(..))
import qualified Graphics.UI.GLFW as GLFW

import qualified Codec.Picture       as Juicy
import qualified Codec.Picture.Types as Juicy

import Control.Monad.Trans.Either
import Control.Monad.Trans.Class (lift)

import Control.Applicative
import Control.Monad
import Control.Arrow (first)
import Control.Lens hiding (transform)
import Control.Monad.IO.Class

import Linear hiding (perspective)

import Leibniz (π, rad, deg)

import Cartesian.Core (BoundingBox(..))

import Geometry.Sculptor.Shapes hiding (Face)

import           Graphics.WaveFront
import qualified Graphics.WaveFront.Load  as Load



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
data Mesh os p = Mesh {
  fVertices  :: Buffer os (B4 Float, B2 Float),
  fPrimitive :: PrimitiveTopology p,
  fTexture   :: Texture2D os (Format RGBFloat)
}


-- |
data Entity os p = Entity {
  fMesh      :: Mesh os p,
  fTransform :: M44 Float
}


-- |
data App os p = App {
  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B Float)),

  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fMatrixValues :: [M44 Float],
  fScalarValues :: [Float],

  fEntities :: [Entity os p]
}


-- |
data ShaderData os p prim = ShaderData {
  fPrimitiveArray :: PrimitiveArray prim (B4 Float, B2 Float),
  fEntity         :: Entity os p,
  fRasterOptions  :: (Side, ViewPort, DepthRange)
}


makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Entity

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''ShaderData


type AppFormat = ContextFormat RGBFloat Depth
type AppT os a = ContextT GLFWWindow os AppFormat IO a

appFormat = ContextFormatColorDepth RGB8 Depth32



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Textures --------------------------------------------------------------------------------------------------------------------------------

-- TODO: Don't hard-code pixel type

-- |
pixel :: Juicy.ColorSpaceConvertible c Juicy.PixelRGB8 => [V3 Juicy.Pixel8] -> a -> b -> c -> [V3 Juicy.Pixel8]
pixel xs _ _ pix = let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs


-- |
-- TODO: Clean this up
loadTexture :: FilePath -> AppT os (Either a (Texture2D os (Format RGBFloat)))
loadTexture fn = runEitherT $ do
  (Juicy.ImageRGB8 image) <- EitherT (liftIO $ Juicy.readImage fn)
  let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
  tex <- EitherT $ textureFromPixels size (Juicy.pixelFold pixel [] image)
  return tex


-- |
-- TODO: Make sure the size is correct
textureFromPixels :: V2 Int -> [V3 Juicy.Pixel8] -> AppT os (Texture2D os (Format RGBFloat))
textureFromPixels size  pixels = do
  -- TODO: What the hell is 'maxBound' doing here (?)
  tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
  writeTexture2D tex 0 0 size pixels
  generateTexture2DMipmap tex
  return tex


-- | Creates a monochrome texture
monochrome :: V2 Int -> V3 Float -> AppT os (Texture2D os (Format RGBFloat))
monochrome size@(V2 dx dy) colour = textureFromPixels size (replicate (dx*dy) colour)

-- Meshes ----------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: Rename (?)
newOBJMesh :: FilePath -> AppT os (Either String (Mesh os Triangles))
newOBJMesh fn = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  EitherT $ fromOBJModel (takeDirectory fn) model


-- | Useful for displaying bounding boxes
-- newWireframe :: BoundingBox (V3 Float) -> AppT os (Mesh os _)
-- newWireframe box = do
--   tex <- monochrome (V2 4 4) (V3 1 1 1) -- White texture


-- |
-- TODO: Deal with missing textures
-- TODO: Get rid of the lens constraints
fromOBJModel :: (HasFaces     (Model f Text i Vector) (Vector (Face Float Text Int Vector)),
                 HasTexcoords (Model f Text i Vector) (Vector (V2 Float)),
                 HasVertices  (Model f Text i Vector) (Vector (V3 Float)))

             => FilePath -> Model f Text i Vector -> AppT os (Either String (Mesh os Triangles))
fromOBJModel root model = runEitherT $ do
  -- Create texture
  tex <- EitherT $ maybe
                     (fmap Right $ monochrome (V2 4 4) (V3 1 1 1)) -- Pure white
                     (loadTexture . (texpath </>))                 -- Load texture
                     (texname model)                               -- Maybe a texture name

  lift $ do
    vertexBuffer :: Buffer os (B4 Float, B2 Float) <- newBuffer (length vs)
    writeBuffer vertexBuffer 0 $ zipWith (\(V3 x y z) (V2 tx ty) -> (V4 x y z 1, V2 (1-tx) ty)) (toList vs) (toList ts)

    return $ Mesh { fVertices  = vertexBuffer,
                    fPrimitive = TriangleList,
                    fTexture   = tex }
  where
    texpath = root </> "textures/"
    texname = listToMaybe . map T.unpack . toList . textures

    vIndex coords i  = coords !? (i-1)
    tIndex coords mi = fromMaybe (V2 0 0) $ mi >>= \i -> (coords !? (i-1))
    (Just vs) = sequence $ fromFaceIndices (model^.vertices)  (vIndex) (^.ivertex)   (model^.faces)
    ts        =            fromFaceIndices (model^.texcoords) (tIndex) (^.itexcoord) (model^.faces)
    cs        = diffuseColours (model^.faces)

--------------------------------------------------------------------------------------------------------------------------------------------

-- | Monadic no-op
pass :: Monad m => m ()
pass = return ()


-- |
-- tick :: 
tick dt app' = do
  size@(V2 cx' cy') <- uncurry V2 <$> withContextWindow (GLFW.getWindowSize . getGLFWWindow)
  mouse             <- uncurry V2 <$> Context.getCursorPos
  
  -- let (V3 x' y' z') = position mouse size
  let (V2 mx my) = fmap realToFrac mouse
      (V2 cx cy) = fmap fromIntegral size

  (Just t) <- fmap realToFrac <$> liftIO GLFW.getTime

  let app = app' & scalarValues.ix 0 .~ (5 * mx/my)
                 & matrixValues.ix 1 .~ perspective (60 * π/180) (cx/cy) 1 50
                 & matrixValues.ix 0 .~ mkTransformationMat (fromQuaternion $ axisAngle (V3 0 1 0) (rad $ t*0.2*360)) (position mouse size)
                 & rasterOptions     .~ (FrontAndBack, ViewPort (V2 0 0) size, DepthRange 0 50)
    
  -- writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues)
  -- writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)

  return app
  where
    position mouse size = let (V2 mx my) = fmap realToFrac mouse
                              (V2 cx cy) = fmap fromIntegral size in V3 0 (30*((-0.5) + mx/cx)) ((-30)*my/cy)


-- |
-- loop :: App os f -> IO ()
loop shader dt app' = do

  -- TODO: FPS throttle (?)
  app <- tick dt app'
  
  render $ do
    clearContextColor (V3 0.78 0.78 0.78)
    clearContextDepth 1.0

  forM (app^.entities) $ \e -> do
    -- Set uniforms
    writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues ++ [e^.transform])
    writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)

    -- Render
    render $ do
      vertexArray <- newVertexArray (e^.mesh.vertices)
      shader $ ShaderData { fPrimitiveArray = toPrimitiveArray (e^.mesh.primitive) vertexArray,
                            fRasterOptions  = app^.rasterOptions,
                            fEntity         = e }
    pass
  swapContextBuffers

  closeRequested <- Context.windowShouldClose
  unless closeRequested $
    loop shader 0 app -- TODO: Fix dt

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  -- Render
  runContextT Context.newContext appFormat $ do
    -- Load all the things
    let root = "C:/Users/Jonatan/Desktop/Haskell/modules/wavefront-render"

    (Right object) <- newOBJMesh (root </> "assets/models/minecraft1.obj")

    scalars  :: Buffer os (Uniform (B Float))       <- newBuffer 3
    matrices :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 2

    shader <- compileShader $ do
      primitiveStream <- toPrimitiveStream (^.primitiveArray)
      
      -- TODO: Structured way of dealing with uniforms
      [mv, pv, tr] <- mapM (\i -> getUniform $ const (matrices, i)) [0,1,2]

      let primitiveStream2 = fmap (\(p, t) -> (pv !*! (mv !*! tr) !* p, t)) primitiveStream

      fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

      let filterMode = SamplerFilter Linear Linear Linear (Just 4)
          edgeMode   = (pure ClampToEdge, undefined)

      samp <- newSampler2D (\sh -> (sh^.entity.mesh.texture, filterMode, edgeMode))

      let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
          fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
          fragmentStream3 = fmap (\(p, d) -> (sampleTexture p, d)) fragmentStream2

      drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream3

    loop shader 0 $ App { fScalarUniforms = scalars,
                          fMatrixUniforms = matrices,

                          fEntities = [Entity { fMesh = object, fTransform = identity }],

                          fScalarValues = [0,0,0],
                          fMatrixValues = [mkTransformationMat (fromQuaternion $ axisAngle (V3 1 0 0) (0)) (V3 0 0 0),
                                           perspective (60 * π/180) (1) 1 1000] }