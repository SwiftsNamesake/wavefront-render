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
import           Data.Foldable (toList)
import           Data.List     (find)
import           Data.Maybe    (fromMaybe, listToMaybe, isJust)
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T
import           Data.Vector (Vector, (!), (!?))
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS

import System.FilePath
import System.IO (stdout, hFlush)

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

import Cartesian.Core as C (BoundingBox (..), corner, size, x, y, z)

import Geometry.Sculptor.Shapes hiding (Face)

import           Graphics.WaveFront
import qualified Graphics.WaveFront.Load            as Load



--------------------------------------------------------------------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------------------------------------------------------------------

-- |
type AppFormat = ContextFormat RGBFloat Depth
type AppT os a = ContextT GLFWWindow os AppFormat IO a

type MeshName = V3 Float

appFormat = ContextFormatColorDepth RGB8 Depth32


-- TODO: Use less rigid constructors (eg. GADTs)

-- |
data Mesh os p = Mesh {
  fVertices  :: Buffer os (B4 Float, B2 Float, B3 Float),
  fPrimitive :: PrimitiveTopology p,
  fTexture   :: Texture2D os (Format RGBFloat),
  fMeshName  :: Maybe MeshName
}


-- |
data Entity os p = Entity {
  fMesh      :: Mesh os p,
  fBox       :: Mesh os Lines, -- TODO: Move this
  fTransform :: M44 Float,
  fTick      :: (Entity os p -> Float -> Entity os p)
}


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


-- |
data Interface os p = Interface {
 fMinimap  :: Mesh os p,
 fMiniSize :: V2 Int
}


-- |
data ShaderData os p = ShaderData {
  fPrimitiveArray :: PrimitiveArray p (B4 Float, B2 Float, B3 Float),
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
-- saveTexture :: FilePath -> Texture2D os (HostFormat (BufferColor (Color a (ColorElement a)) a)) -> (a -> V3 Float) -> AppT os ()
saveTexture :: FilePath -> Texture2D os (Format RGBFloat) -> (V3 Float -> V3 Float) -> AppT os ()
saveTexture fn tex f = do
  pixels <- readTexture2D tex 0 (V2 0 0) size (\ps c -> return $ convert (f c) ++ ps) []
  liftIO $ Juicy.savePngImage fn (Juicy.ImageRGB8 $ image size pixels)
  where
    (size:_) = texture2DSizes tex
    image (V2 dx dy) pixels = Juicy.Image { Juicy.imageWidth = dx, Juicy.imageHeight = dy, Juicy.imageData = VS.fromList pixels }
    pixel8    = floor . (*255)
    convert c = let (V3 r g b) = fmap pixel8 c in [r, g, b]


-- |
-- TODO: Clean this up
loadTexture :: FilePath -> AppT os (Either String (Texture2D os (Format RGBFloat)))
loadTexture fn = runEitherT $ do
  (Juicy.ImageRGB8 image) <- EitherT (liftIO $ Juicy.readImage fn)
  let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
  tex <- EitherT . fmap Right $ textureFromPixels size (Juicy.pixelFold pixel [] image)
  return tex


-- |
newTexture :: V2 Int -> (Int -> Int -> V3 Juicy.Pixel8) -> AppT os (Texture2D os (Format RGBFloat))
newTexture size@(V2 dx dy) f = textureFromPixels size [f x y | x <- [0..dx-1], y <- [0..dy-1]]


-- |
pixelAt :: V2 Int -> Texture2D os (Format RGBFloat) -> ContextT GLFWWindow os (ContextFormat RGBFloat a) IO (V3 Float)
pixelAt (V2 x y) tex = readTexture2D tex 0 (V2 x y) (V2 1 1) (\_ c -> return c) (V3 0 0 0)


-- |
-- TODO: Figure out order and format
-- TODO: Make polymorphic
readPixels :: V2 Int -> V2 Int -> Texture2D os (Format RGBFloat) -> ContextT GLFWWindow os (ContextFormat RGBFloat a) IO [V3 Float]
readPixels from size tex = readTexture2D tex 0 from size (\ps c -> return (c:ps)) []


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
newOBJMesh :: MeshName -> FilePath -> AppT os (Either String (Mesh os Triangles))
newOBJMesh name fn = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  EitherT $ fromOBJModel name (takeDirectory fn) model


-- |
newQuadXY :: MeshName -> V2 Int -> (V3 Float -> V2 Float) -> (V3 Float -> V3 Float) -> Texture2D os (Format RGBFloat) -> AppT os (Mesh os Triangles)
newQuadXY name size texcoord colour tex = do

  vertexBuffer :: Buffer os (B4 Float, B2 Float, B3 Float) <- newBuffer (length vertices)
  writeBuffer vertexBuffer 0 vertices

  liftIO $ mapM (print . (^._2)) vertices

  return $ Mesh { fVertices  = vertexBuffer,
                  fPrimitive = TriangleList,
                  fTexture   = tex,
                  fMeshName  = Just name }
  where
    (V2 dx dy) = fmap fromIntegral size
    makeVertex v = (to4D 1 v, texcoord v, colour v)
    vertices = map makeVertex (concat . triangles $ planeXY V3 dx dy)


-- |
newSpriteEntity :: MeshName -> [FilePath] -> Float -> M44 Float -> AppT os (Either String (Entity os Triangles))
newSpriteEntity name fns fps tr = runEitherT $ do
  frames <- mapM (EitherT . loadTexture) fns
  start  <- hoistEither $ safeHead frames
  size <- hoistEither $ safeHead (texture2DSizes start)
  mesh <- lift $ newQuadXY name size texcoord (const $ pure 0) start
  wire <- lift $ newWireframe (let (V2 dx dy) = fmap fromIntegral size in BoundingBox { cornerOf = V3 (-dx/2) (-dy/2) 0, sizeOf = V3 dx dy 0 })
  return $ Entity { fMesh      = mesh,
                    fTransform = tr,
                    fBox       = wire,
                    fTick      = (\self _ -> nextFrame (drop 1 . cycle $ frames) self) }
  where
    texcoord = fmap signum . (+ (V2 0.5 0.5)) . to2D
    nextFrame frames self = self & mesh.texture .~ head frames
                                 & tick         .~ (\self _ -> nextFrame (drop 1 frames) self)


-- |
newOBJEntity :: MeshName -> FilePath -> M44 Float -> AppT os (Either String (Entity os Triangles))
newOBJEntity name fn tr = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  mesh <- EitherT $ fromOBJModel name (takeDirectory fn) model
  wire <- lift $ newWireframe (bounds model)
  return $ Entity { fMesh      = mesh,
                    fBox       = wire,
                    fTransform = tr,
                    fTick      = (\self _ -> self) }


-- | Useful for displaying bounding boxes
newWireframe :: BoundingBox (V3 Float) -> AppT os (Mesh os Lines)
newWireframe box = do
  tex <- monochrome (V2 4 4) (V3 0 0 0) -- Black texture
  let ls = concatMap makeLine $ zipWith (\(fr,to) c -> (fr,to,c)) cuboidLineIndices colours

  vertexBuffer :: Buffer os (B4 Float, B2 Float, B3 Float) <- newBuffer (length ls)
  writeBuffer vertexBuffer 0 ls

  return $ Mesh { fVertices  = vertexBuffer,
                  fPrimitive = LineList,
                  fTexture   = tex,
                  fMeshName  = Nothing }
  where
    (V3 dx dy dz) = box^.size
    vs      = cuboid (\x y z -> (box^.corner) + (box^.size)*0.5 + V3 x y z) dx dy dz
    colours = concatMap (replicate 4) [V3 1 0 0, V3 0 0 1, V3 0 1 0] -- X is Red, Y is Blue, Z is Green
    makeLine (fr,to,c) = [(to4D 1 $ vs !! fr, V2 0 0, c), (to4D 1 $ vs !! to, V2 0 0, c)]


-- |
-- TODO: Deal with missing textures
-- TODO: Get rid of the lens constraints
fromOBJModel :: MeshName -> FilePath -> Model Float Text Int Vector -> AppT os (Either String (Mesh os Triangles))
fromOBJModel name root model = runEitherT $ do
  -- Create texture
  tex <- EitherT $ maybe
                     (fmap Right $ monochrome (V2 4 4) (pure maxBound)) -- Pure white
                     (loadTexture . (texpath </>))                      -- Load texture
                     (texname model)                                    -- Maybe a texture name

  lift $ do
    -- TODO: Deal with missing or leftover values
    vertexBuffer :: Buffer os (B4 Float, B2 Float, B3 Float) <- newBuffer (length vs)
    writeBuffer vertexBuffer 0 $ zipWith3 (makeVertex) (toList vs) (toList ts) (toList cs)

    return $ Mesh { fVertices  = vertexBuffer,
                    fPrimitive = TriangleList,
                    fTexture   = tex,
                    fMeshName  = Just name }
  where
    makeVertex v (V2 tx ty) (Colour r g b _) = (to4D 1 v, V2 (1-tx) ty, V3 r g b)

    texpath = root </> "textures/"
    texname = listToMaybe . map T.unpack . toList . textures

    vIndex coords i  = coords !? (i-1)
    tIndex coords mi = fromMaybe (V2 0 0) $ mi >>= \i -> (coords !? (i-1))
    (Just vs) = sequence $ fromFaceIndices (model^.vertices)  (vIndex) (^.ivertex)   (model^.faces)
    ts        =            fromFaceIndices (model^.texcoords) (tIndex) (^.itexcoord) (model^.faces)
    cs        = diffuseColours (model^.faces)

-- Shaders ---------------------------------------------------------------------------------------------------------------------------------

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

  let primitiveStream2 = fmap (\(p, t, c) -> (pv !*! (mv !*! tr) !* p, (t, c))) primitiveStream

  fragmentStream <- rasterize (^.rasterOptions) primitiveStream2

  samp <- newSampler2D (\sh -> (sh^.texture, sh^.filterMode, (pure ClampToEdge, 0)))

  let sampleTexture   = sample2D samp SampleAuto Nothing Nothing
      fragmentStream2 = withRasterizedInfo (\ a (RasterizedInfo { rasterizedFragCoord = V4 _ _ z' _ }) -> (a, z')) fragmentStream
      fragmentStream3 = fmap (\((p, c), d) -> (sampleTexture p + c, d)) fragmentStream2
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

-- Bits and bobs ---------------------------------------------------------------------------------------------------------------------------

-- | Monadic no-op
pass :: Monad m => m ()
pass = return ()


-- |
around :: (Num f, Ord f) => f -> f -> f -> Bool
around delta a b = abs (a-b) < delta


-- | Safely behead a list
safeHead :: [a] -> Either String a
safeHead (x:_) = Right x
safeHead ([])  = Left "safeHead: empty list"


-- |
to2D :: V3 a -> V2 a
to2D (V3 x y _) = V2 x y


-- |
to3D :: a -> V2 a -> V3 a
to3D z (V2 x y) = V3 x y z


-- |
to4D :: a -> V3 a -> V4 a
to4D w (V3 x y z) = V4 x y z w

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- appTick ::
appTick dt app' = do
  size@(V2 cx' cy') <- uncurry V2 <$> withContextWindow (GLFW.getWindowSize . getGLFWWindow)
  mouse             <- uncurry V2 <$> Context.getCursorPos
  leftDown          <- fmap (== GLFW.MouseButtonState'Pressed) (Context.getMouseButton GLFW.MouseButton'1)

  let (V2 mx my) = fmap realToFrac mouse
      (V2 cx cy) = fmap fromIntegral size

  (Just t) <- fmap realToFrac <$> liftIO GLFW.getTime

  let app = app' & scalarValues.ix 0 .~ (5 * mx/my)
                 & perspectiveOf .~ perspective (60 * π/180) (cx/cy) 1 50
                 & modelviewOf   .~ mkTransformationMat (fromQuaternion $ axisAngle (V3 0 1 0) (rad $ t*0.2*360)) (position mouse size)
                 & rasterOptions .~ (FrontAndBack, ViewPort (V2 0 0) size, DepthRange 0 50)
                 & windowSize    .~ V2 cx cy
                 & entities      %~ map (\self -> (self^.tick) self dt)

  when (leftDown) $ do
    which <- pixelAt (fmap floor mouse & y %~ (cy' -)) (app^.silhouettes)
    liftIO (print (nameThatMesh which) >> hFlush stdout)

  return app
  where
    nameThatMesh c = fmap snd $ find ((< 0.05) . sum . fmap abs . liftA2 (-) c . fst) [(V3 1 0 0, "minecraft"), (V3 0 1 0, "gourd"), (V3 0 0 1, "text")]
    position mouse size = let (V2 mx my) = fmap realToFrac mouse
                              (V2 cx cy) = fmap fromIntegral size in V3 0 (30*((-0.5) + mx/cx)) ((-30)*my/cy)


-- |
-- loop :: App os f -> IO ()
loop (solid, wire, flat) dt app' = do

  -- TODO: FPS throttle (?)
  app <- appTick dt app'

  render $ do
    clearContextColor (V3 0.78 0.78 0.78)
    clearContextDepth 1.0

    contours <- getTexture2DImage (app^.silhouettes)  0
    depths   <- getTexture2DImage (app^.depthTexture) 0 -- TODO: Get rid of this (?)

    clearColorImage contours (V3 0 0 0)
    clearDepthImage depths   (1.0)

  forM (app^.entities) $ \e -> do
    -- Set uniforms
    writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues ++ [e^.transform])
    writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)
    writeBuffer (app^.vectorUniforms) 0 [fromMaybe 0 $ e^.mesh.meshName]

    -- Render
    render $ do
      vertexArray <- newVertexArray (e^.mesh.vertices)
      boundsArray <- newVertexArray (e^.box.vertices)

      contours <- getTexture2DImage (app^.silhouettes)  0
      depths   <- getTexture2DImage (app^.depthTexture) 0 -- TODO: Get rid of this (?)

      solid $ ShaderData { fPrimitiveArray = toPrimitiveArray (e^.mesh.primitive) vertexArray,
                           fMatrixUniforms = app^.matrixUniforms,
                           fScalarUniforms = app^.scalarUniforms,
                           fVectorUniforms = app^.vectorUniforms,

                           fFilterMode = SamplerFilter Linear Linear Linear (Just 4),
                           fEdgeMode   = (pure ClampToEdge), --, pure 1),

                           fMeshName = e^.mesh.meshName,
                           fSilhouettes   = contours,
                           fDepthTexture  = depths,

                           fTexture = e^.mesh.texture,

                           fRasterOptions  = app^.rasterOptions }
      
      wire $ ShaderData { fPrimitiveArray = toPrimitiveArray (e^.box.primitive) boundsArray,
                          fMatrixUniforms = app^.matrixUniforms,
                          fScalarUniforms = app^.scalarUniforms,
                          fVectorUniforms = app^.vectorUniforms,

                          fFilterMode = SamplerFilter Linear Linear Linear (Just 4),
                          fEdgeMode   = (pure ClampToEdge), --, pure 1),

                          fMeshName = e^.box.meshName,
                          fSilhouettes   = contours,
                          fDepthTexture  = depths,

                          fTexture = e^.box.texture,

                          fRasterOptions  = app^.rasterOptions }

  let (V2 cx cy) = app^.windowSize
      (V2 hx hy) = fmap fromIntegral (app^.interface.miniSize)

  writeBuffer (app^.matrixUniforms) 0 ([identity, ortho 0 cx 0 cy 0 1, identity & translation .~ V3 (hx+10) (cy-hy-10) (-0.5)])
  writeBuffer (app^.scalarUniforms) 0 ([0,0,0])
  writeBuffer (app^.vectorUniforms) 0 ([0,0,0])

  render $ do
    let mini = app^.interface.minimap
    vertexArray <- newVertexArray (mini^.vertices)

    flat $ ShaderData { fPrimitiveArray = toPrimitiveArray (mini^.primitive) vertexArray,
                        fMatrixUniforms = app^.matrixUniforms,
                        fScalarUniforms = app^.scalarUniforms,
                        fVectorUniforms = app^.vectorUniforms,

                        fFilterMode = SamplerFilter Linear Linear Linear (Just 4),
                        fEdgeMode   = (pure ClampToEdge), --, pure 1),

                        fMeshName = mini^.meshName,
                         --fSilhouettes   = contours,
                         --fDepthTexture  = depths,

                        fTexture = mini^.texture,

                        fRasterOptions  = app^.rasterOptions }

    pass
  swapContextBuffers

  closeRequested <- Context.windowShouldClose
  unless closeRequested $
    loop (solid, wire, flat) 0 app -- TODO: Fix dt

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
main :: IO ()
main = do

  -- Render
  runContextT Context.newContext appFormat $ do
    -- Load all the things
    let root = "C:/Users/Jonatan/Desktop/Haskell/modules/wavefront-render"
    
    -- Uniform buffers
    -- This is so frail
    scalars  :: Buffer os (Uniform (B Float))       <- newBuffer 3
    matrices :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 3
    vectors  :: Buffer os (Uniform (B3 Float))      <- newBuffer 3
    
    -- Contour textures
    -- TODO: Deal with resizes
    contours <- newTexture2D SRGB8   (V2 720 480) 1
    depths   <- newTexture2D Depth16 (V2 720 480) 1

    checkers <- newTexture (V2 16 16) (\x y -> if mod x 2 == mod y 2 then pure maxBound else pure 0)

    -- Entities and meshes
    let miniSize = V2 180 120
    mini <- newQuadXY (V3 0 0 0) (miniSize) (fmap ((*0.5) . (+1) . signum) . to2D) (const $ pure 0) contours

    (Right minecraft) <- newOBJEntity (V3 1 0 0) (root </> "assets/models/minecraft1.obj")    (identity)
    (Right gourd)     <- newOBJEntity (V3 0 1 0) (root </> "assets/models/extruded-text.obj") (mkTransformationMat
                                                                                                (fromQuaternion $ axisAngle (V3 1 0 0) (π*0.5))
                                                                                                (V3 0 8 0))
    (Right text)      <- newOBJEntity (V3 0 0 1) (root </> "assets/models/frodo.obj")         (mkTransformationMat
                                                                                                (fromQuaternion $ axisAngle (V3 1 0 0) (π*0.5))
                                                                                                (V3 0 5 0))
    
    -- TODO: How do you use the same shader for different topologies?
    solid <- compileShader newContextShader
    wire  <- compileShader newContextShader
    flat  <- compileShader newInterfaceShader

    loop (solid, wire, flat) 0 $ App { fRasterOptions  = (FrontAndBack, ViewPort (V2 0 0) (V2 720 480), DepthRange 0 50),
                                       fWindowSize     = (V2 720 480),
                                       fScalarUniforms = scalars,
                                       fMatrixUniforms = matrices,
                                       fVectorUniforms = vectors,
                                       fEntities  = [text, minecraft, gourd],
                                       fInterface = Interface { fMinimap = mini, fMiniSize = miniSize },
                                       fScalarValues = [0,0,0],
                                       fShowBounds   = True,
                                       fSilhouettes  = contours,
                                       fDepthTexture = depths,
                                       fVectorValues = [],
                                       fMatrixValues = [mkTransformationMat (fromQuaternion $ axisAngle (V3 1 0 0) (0)) (V3 0 0 0),
                                                        perspective (60 * π/180) (1) 1 1000] }
    
    --
    saveTexture "contours.png" (contours) (id)