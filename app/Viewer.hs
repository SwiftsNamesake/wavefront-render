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



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import           Data.Foldable                      (toList)
import           Data.Maybe                         (fromMaybe, listToMaybe)
import           Data.Monoid
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Vector                        (Vector, (!), (!?))
import qualified Data.Vector                        as V

import System.FilePath

import Text.Printf

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW        as Context
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))
import qualified Graphics.UI.GLFW                   as GLFW

import qualified Codec.Picture       as Juicy
import qualified Codec.Picture.Types as Juicy

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Either

import Control.Applicative
import Control.Arrow       (first)
import Control.Lens hiding (transform)
import Control.Monad
import Control.Monad.IO.Class

import Linear hiding (perspective)

import Leibniz (deg, rad, π)

import Cartesian.Core as C (BoundingBox (..), corner, size)

import Geometry.Sculptor.Shapes hiding (Face)

import           Graphics.WaveFront
import qualified Graphics.WaveFront.Load            as Load



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Use less rigid constructors (eg. GADTs)

-- |
data Mesh os p = Mesh {
  fVertices  :: Buffer os (B4 Float, B2 Float, B3 Float),
  fPrimitive :: PrimitiveTopology p,
  fTexture   :: Texture2D os (Format RGBFloat)
}


-- |
data Entity os p = Entity {
  fMesh      :: Mesh os p,
  fBox       :: Mesh os Lines, -- TODO: Move this
  fTransform :: M44 Float
}


-- |
data App os p = App {
  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B Float)),

  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fMatrixValues   :: [M44 Float],
  fScalarValues   :: [Float],

  fEntities       :: [Entity os p],

  fShowBounds :: Bool
}


-- |
data ShaderData os p = ShaderData {
  fPrimitiveArray :: PrimitiveArray p (B4 Float, B2 Float, B3 Float),
  fRasterOptions  :: (Side, ViewPort, DepthRange),

  fMatrixUniforms :: Buffer os (Uniform (M44 (B Float))),
  fScalarUniforms :: Buffer os (Uniform (B Float)),

  fTexture   :: Texture2D os (Format RGBFloat)
}


makeLensesWith abbreviatedFields ''Mesh
makeLensesWith abbreviatedFields ''Entity

makeLensesWith abbreviatedFields ''App
makeLensesWith abbreviatedFields ''ShaderData


-- | Lens synonyms
perspectiveOf :: Simple Traversal (App os p) (M44 Float)
perspectiveOf = matrixValues.ix 1

modelviewOf :: Simple Traversal (App os p) (M44 Float)
modelviewOf = matrixValues.ix 0


-- |
type AppFormat = ContextFormat RGBFloat Depth
type AppT os a = ContextT GLFWWindow os AppFormat IO a

appFormat = ContextFormatColorDepth RGB8 Depth32



--------------------------------------------------------------------------------------------------------------------------------------------
-- Functions
--------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------


-- Textures --------------------------------------------------------------------------------------------------------------------------------

-- TODO: Don't hard-code pixel type

-- |
pixel :: Juicy.ColorSpaceConvertible c Juicy.PixelRGB8 => [V3 Juicy.Pixel8] -> a -> b -> c -> [V3 Juicy.Pixel8]
pixel xs _ _ pix = let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs


-- |
-- TODO: Clean this up
loadTexture :: FilePath -> AppT os (Either String (Texture2D os (Format RGBFloat)))
loadTexture fn = runEitherT $ do
  (Juicy.ImageRGB8 image) <- EitherT (liftIO $ Juicy.readImage fn)
  let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
  tex <- EitherT . fmap Right $ textureFromPixels size (Juicy.pixelFold pixel [] image)
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
monochrome :: V2 Int -> V3 Juicy.Pixel8 -> AppT os (Texture2D os (Format RGBFloat))
monochrome size@(V2 dx dy) colour = textureFromPixels size (replicate (dx*dy) colour)

-- Meshes ----------------------------------------------------------------------------------------------------------------------------------

-- | Loads an OBJ model and applies some transformation to it
-- withOBJModel :: (Fractional f, Integral i) => FilePath -> (Model f Text i Vector -> AppT os (Either String a)) -> AppT os (Either String a)
-- withOBJModel fn f = runEitherT $ do
--   model <- EitherT $ liftIO (Load.model fn)
--   lift (f model)


-- |
-- TODO: Rename (?)
newOBJMesh :: FilePath -> AppT os (Either String (Mesh os Triangles))
newOBJMesh fn = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  EitherT $ fromOBJModel (takeDirectory fn) model


-- |
newOBJEntity :: FilePath -> M44 Float -> AppT os (Either String (Entity os Triangles))
newOBJEntity fn tr = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  mesh <- EitherT $ fromOBJModel (takeDirectory fn) model
  wire <- lift $ newWireframe (bounds model)
  return $ Entity { fMesh      = mesh,
                    fBox       = wire,
                    fTransform = tr }


-- | Useful for displaying bounding boxes
newWireframe :: BoundingBox (V3 Float) -> AppT os (Mesh os Lines)
newWireframe box = do
  tex <- monochrome (V2 4 4) (V3 0 0 0) -- Black texture
  let ls = concatMap makeLine $ zipWith (\(fr,to) c -> (fr,to,c)) cuboidLineIndices colours

  vertexBuffer :: Buffer os (B4 Float, B2 Float, B3 Float) <- newBuffer (length ls)
  writeBuffer vertexBuffer 0 ls

  return $ Mesh { fVertices  = vertexBuffer,
                  fPrimitive = LineList,
                  fTexture   = tex }
  where
    (V3 dx dy dz) = box^.size
    addW (V3 x y z) = V4 x y z 1
    vs      = cuboid (\x y z -> (box^.corner) + (box^.size)*0.5 + V3 x y z) dx dy dz
    colours = concatMap (replicate 4) [V3 1 0 0, V3 0 0 1, V3 0 1 0] -- X is Red, Y is Blue, Z is Green
    makeLine (fr,to,c) = [(addW $ vs !! fr, V2 0 0, c), (addW $ vs !! to, V2 0 0, c)]


-- |
-- TODO: Deal with missing textures
-- TODO: Get rid of the lens constraints
fromOBJModel :: FilePath -> Model Float Text Int Vector -> AppT os (Either String (Mesh os Triangles))
fromOBJModel root model = runEitherT $ do
  -- Create texture
  tex <- EitherT $ maybe
                     (fmap Right $ monochrome (V2 4 4) (pure maxBound)) -- Pure white
                     (loadTexture . (texpath </>))                      -- Load texture
                     (texname model)                                    -- Maybe a texture name

  lift $ do
    liftIO $ print (V.take 5 $ cs)
    -- TODO: Deal with missing or leftover values
    vertexBuffer :: Buffer os (B4 Float, B2 Float, B3 Float) <- newBuffer (length vs)
    writeBuffer vertexBuffer 0 $ zipWith3 (makeVertex) (toList vs) (toList ts) (toList cs)

    return $ Mesh { fVertices  = vertexBuffer,
                    fPrimitive = TriangleList,
                    fTexture   = tex }
  where
    makeVertex (V3 x y z) (V2 tx ty) (Colour r g b _) = (V4 x y z 1, V2 (1-tx) ty, V3 r g b)

    texpath = root </> "textures/"
    texname = listToMaybe . map T.unpack . toList . textures

    vIndex coords i  = coords !? (i-1)
    tIndex coords mi = fromMaybe (V2 0 0) $ mi >>= \i -> (coords !? (i-1))
    (Just vs) = sequence $ fromFaceIndices (model^.vertices)  (vIndex) (^.ivertex)   (model^.faces)
    ts        =            fromFaceIndices (model^.texcoords) (tIndex) (^.itexcoord) (model^.faces)
    cs        = diffuseColours (model^.faces)

-- Shaders ---------------------------------------------------------------------------------------------------------------------------------

-- |
 -- :: Shader os (ContextFormat c ds)           (ShaderData os p prim) ()
newShader :: Shader os (ContextFormat RGBFloat Depth) (ShaderData os p) ()
newShader = do
  primitiveStream <- toPrimitiveStream (^.primitiveArray)

  -- TODO: Structured way of dealing with uniforms
  [mv, pv, tr] <- mapM (\i -> getUniform $ matrixUniform i) [0,1,2]

  let primitiveStream2 = fmap (\(p, t, c) -> (pv !*! (mv !*! tr) !* p, (t, c))) primitiveStream

  fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

  let filterMode = SamplerFilter Linear Linear Linear (Just 4)
      edgeMode   = (pure ClampToEdge, undefined)

  samp <- newSampler2D (\sh -> (sh^.texture, filterMode, edgeMode))

  let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
      fragmentStream3 = fmap (\((p, c), d) -> (sampleTexture p + c, d)) fragmentStream2

  drawContextColorDepth (const (ContextColorOption NoBlending (pure True), DepthOption Less True)) fragmentStream3
  where
    matrixUniform i sh = (sh^.matrixUniforms, i)

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
                 & perspectiveOf .~ perspective (60 * π/180) (cx/cy) 1 50
                 & modelviewOf   .~ mkTransformationMat (fromQuaternion $ axisAngle (V3 0 1 0) (rad $ t*0.2*360)) (position mouse size)
                 & rasterOptions .~ (FrontAndBack, ViewPort (V2 0 0) size, DepthRange 0 50)

  -- writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues)
  -- writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)

  return app
  where
    position mouse size = let (V2 mx my) = fmap realToFrac mouse
                              (V2 cx cy) = fmap fromIntegral size in V3 0 (30*((-0.5) + mx/cx)) ((-30)*my/cy)


-- |
-- loop :: App os f -> IO ()
loop (solid, wire) dt app' = do

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
      boundsArray <- newVertexArray (e^.box.vertices)
      
      solid $ ShaderData { fPrimitiveArray = toPrimitiveArray (e^.mesh.primitive) vertexArray,
                           fMatrixUniforms = app^.matrixUniforms,
                           fScalarUniforms = app^.scalarUniforms,

                           fTexture = e^.mesh.texture,

                           fRasterOptions  = app^.rasterOptions }
      
      wire $ ShaderData { fPrimitiveArray = toPrimitiveArray (e^.box.primitive) boundsArray,
                          fMatrixUniforms = app^.matrixUniforms,
                          fScalarUniforms = app^.scalarUniforms,

                          fTexture = e^.box.texture,

                          fRasterOptions  = app^.rasterOptions }
    pass
  swapContextBuffers

  closeRequested <- Context.windowShouldClose
  unless closeRequested $
    loop (solid, wire) 0 app -- TODO: Fix dt

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  -- Render
  runContextT Context.newContext appFormat $ do
    -- Load all the things
    let root = "C:/Users/Jonatan/Desktop/Haskell/modules/wavefront-render"

    (Right minecraft) <- newOBJEntity (root </> "assets/models/minecraft1.obj")    (identity)
    (Right gourd)     <- newOBJEntity (root </> "assets/models/extruded-text.obj") (mkTransformationMat
                                                                                      (fromQuaternion $ axisAngle (V3 1 0 0) (π*0.5))
                                                                                      (V3 0 8 0) )
    (Right text)      <- newOBJEntity (root </> "assets/models/frodo.obj")         (mkTransformationMat
                                                                                      (fromQuaternion $ axisAngle (V3 1 0 0) (π*0.5))
                                                                                      (V3 0 5 0) )

    scalars  :: Buffer os (Uniform (B Float))       <- newBuffer 3
    matrices :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 3
    
    -- TODO: How do you use the same shader for different topologies?
    solid <- compileShader newShader
    wire  <- compileShader newShader

    loop (solid, wire) 0 $ App { fScalarUniforms = scalars,
                                 fMatrixUniforms = matrices,
                                 fEntities = [text, minecraft, gourd],
                                 fScalarValues = [0,0,0],
                                 fShowBounds   = True,
                                 fMatrixValues = [mkTransformationMat (fromQuaternion $ axisAngle (V3 1 0 0) (0)) (V3 0 0 0),
                                                  perspective (60 * π/180) (1) 1 1000] }
