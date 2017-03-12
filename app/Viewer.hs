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



-- GHC Directives --------------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}

-- API -------------------------------------------------------------------------------------------------------------------------------------

module Main where

-- We'll need these ------------------------------------------------------------------------------------------------------------------------

import           Data.Foldable (toList)
import           Data.List     (find)
import           Data.Maybe    (fromMaybe, listToMaybe, isJust)
import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Vector as V
import qualified Data.Text   as T
import qualified Data.Set as S
import           Data.Set (Set)

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
import qualified Graphics.WaveFront.Load as Load

import           Viewer.Types
import           Viewer.Trinkets
import qualified Viewer.Texture as Texture
import qualified Viewer.Shader  as Shader
import qualified Viewer.Mesh    as Mesh

-- Definitions -----------------------------------------------------------------------------------------------------------------------------

-- Textures --------------------------------------------------------------------------------------------------------------------------------

-- Meshes ----------------------------------------------------------------------------------------------------------------------------------

-- |
newSpriteEntity :: [FilePath] -> Float -> AppT os (Either String (Entity os Triangles))
newSpriteEntity fns fps = runEitherT $ do
  frames <- mapM (EitherT . loadTexture) fns
  start  <- hoistEither $ safeHead frames
  mesh   <- lift $ newQuadXY size texcoord (const $ pure 0) start
  return $ Entity [Paint (\s self -> mesh), Animate (\_ self -> nextFrame (drop 1 . cycle $ frames) self)]
  where
    nextFrame frames self = self & mesh.texture .~ head frames
                                 & tick         .~ (\self _ -> nextFrame (drop 1 frames) self)


-- |
newOBJEntity :: FilePath -> AppT os (Either String (Entity os Triangles))
newOBJEntity fn = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  mesh  <- EitherT $ fromOBJModel name (takeDirectory fn) model
  wire  <- lift $ newWireframe (bounds model)
  return $ Entity [fMesh = mesh, fBox = wire]


-- | Useful for displaying bounding boxes
newWireframe :: BoundingBox (V3 Float) -> AppT os (Mesh os Lines)
newWireframe box = do
  tex <- monochrome (V2 4 4) (V3 0 0 0) -- Black texture
  let ls = concatMap makeLine $ zipWith (\(fr,to) c -> (fr,to,c)) cuboidLineIndices colours

  vertexBuffer :: Buffer os VertexAttribute <- newBuffer (length ls)
  writeBuffer vertexBuffer 0 ls

  return $ Mesh { fVertices  = vertexBuffer,
                  fPrimitive = LineList,
                  fTexture   = tex }
  where
    (V3 dx dy dz) = box^.size
    vs      = cuboid (\x y z -> (box^.corner) + (box^.size)*0.5 + V3 x y z) dx dy dz
    colours = concatMap (replicate 4) [V3 1 0 0, V3 0 0 1, V3 0 1 0] -- X is Red, Y is Blue, Z is Green
    makeLine (fr,to,c) = [(to4D 1 $ vs !! fr, V3 1 0 0,  V2 0 0, c), (to4D 1 $ vs !! to, V3 1 0 0, V2 0 0, c)]

-- Shaders ---------------------------------------------------------------------------------------------------------------------------------

-- Bits and bobs ---------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------

-- |
-- TODO: We really need a better way of dealing with input
appTick dt app' = do
  size@(V2 cx' cy') <- uncurry V2 <$> withContextWindow (GLFW.getWindowSize . getGLFWWindow)
  mouse             <- uncurry V2 <$> Context.getCursorPos
  leftDown          <- fmap (== GLFW.MouseButtonState'Pressed) (Context.getMouseButton GLFW.MouseButton'1)

  let (V2 mx my) = fmap realToFrac mouse
      (V2 cx cy) = fmap fromIntegral size

  (Just t) <- fmap realToFrac <$> liftIO GLFW.getTime

  let app = app' & uniformValues.scalars.ix 0 .~ (5 * mx/my)
                 & perspectiveOf .~ perspective (60 * π/180) (cx/cy) 1 50
                 & modelviewOf   .~ mkTransformationMat (fromQuaternion $ axisAngle (V3 0 1 0) (rad $ t*0.2*360)) (position mouse size)
                 & rasterOptions .~ (FrontAndBack, ViewPort (V2 0 0) size, DepthRange 0 50)
                 & windowSize    .~ V2 cx cy
                 & entities      %~ map (\self -> (self^.tick) self dt)

  when (leftDown) $ do
    (Just which) <- pixelAt (fmap floor mouse & y %~ (cy' -)) (app^.silhouettes)
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
    clearContextColor (fmap (/255) $ V3 135 206 235)

    clearContextDepth 1.0

    contours <- getTexture2DImage (app^.silhouettes)  0
    depths   <- getTexture2DImage (app^.depthTexture) 0 -- TODO: Get rid of this (?)

    clearColorImage contours (V3 0 0 0)
    clearDepthImage depths   (1.0)

  forM (app^.entities) $ \e -> do
    -- Set uniforms
    writeBuffer (app^.matrixUniforms) 0 (app^.matrixValues ++ [e^.transform])
    writeBuffer (app^.scalarUniforms) 0 (app^.scalarValues)
    writeBuffer (app^.vectorUniforms) 0 [fromMaybe 0 $ e^.mesh.name]

    -- Render
    render $ do
      vertexArray <- newVertexArray (e^.mesh.vertices)
      boundsArray <- newVertexArray (e^.box.vertices)

      contours <- getTexture2DImage (app^.silhouettes)  0
      depths   <- getTexture2DImage (app^.depthTexture) 0 -- TODO: Get rid of this (?)

      solid $ Shader.makeShaderData
      wire  $ Shader.makeShaderData

  -- Render the 'interface'
  let (V2 cx cy) = app^.windowSize
      (V2 hx hy) = fmap ((*0.5) . fromIntegral) (app^.interface.miniSize)

  writeBuffer (app^.matrixUniforms) 0 ([identity, ortho 0 cx 0 cy 0 1, identity & translation .~ V3 (hx+10) (cy-hy-10) (-0.5)])
  writeBuffer (app^.scalarUniforms) 0 ([0,0,0])
  writeBuffer (app^.vectorUniforms) 0 ([0,0,0])

  render $ do
    let mini = app^.interface.minimap
    vertexArray <- newVertexArray (mini^.vertices)

    flat $ CommonShaderData { fPrimitiveArray = toPrimitiveArray (mini^.primitive) vertexArray,
                              fUniforms = app^.uniformBuffers,
                              fRasterOptions  = app^.rasterOptions,

                              fTexture = TextureData { fFilterMode = SamplerFilter Linear Linear Linear (Just 4),
                                                       fEdgeMode   = (pure ClampToEdge), --, pure 1),
                                                       fTexture    = mini^.texture } }
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
  runContextT Context.newContext (ContextFormatColorDepth RGB8 Depth32) $ do
    -- Load all the things
    let root = "C:/Users/Jonatan/Desktop/Haskell/modules/wavefront-render"

    -- Uniform buffers
    -- TODO: This is so frail
    scalars'  :: Buffer os (Uniform (B Float))       <- newBuffer 3
    matrices' :: Buffer os (Uniform (M44 (B Float))) <- newBuffer 3
    vectors'  :: Buffer os (Uniform (B3 Float))      <- newBuffer 3

    -- Contour textures
    -- TODO: Deal with resizes
    contours <- newTexture2D SRGB8   (V2 720 480) 1
    depths   <- newTexture2D Depth16 (V2 720 480) 1

    -- Entities and meshes
    let miniSize = V2 180 120
    mini <- newQuadXY (V3 0 0 0) (miniSize) (texcoord) (const $ pure 0) contours

    (Right minecraft) <- newOBJEntity (V3 1 0 0) (root </> "assets/models/minecraft1.obj")    (identity)
    (Right gourd)     <- newOBJEntity (V3 0 1 0) (root </> "assets/models/extruded-text.obj") (mkTransformationMat
                                                                                                (axisRotation (V3 1 0 0) (π*0.5))
                                                                                                (V3 0 8 0))
    (Right text)      <- newOBJEntity (V3 0 0 1) (root </> "assets/models/frodo.obj")         (mkTransformationMat
                                                                                                (axisRotation (V3 1 0 0) (π*0.5))
                                                                                                (V3 0 5 0))

    -- TODO: How do you use the same shader for different topologies?
    solid <- compileShader Shader.context
    wire  <- compileShader Shader.context
    flat  <- compileShader Shader.interface

    loop 0 $ App { fRasterOptions = (FrontAndBack, ViewPort (V2 20 20) (V2 720 480 - V2 20 20), DepthRange 0 50),
                   fWindowSize    = (V2 720 480 - 2 * V2 20 20),
                   fInterface     = Interface { fMinimap = mini, fMiniSize = miniSize },

                   fShaders = Shaders {
                     fSolid     = solid,
                     fWire      = wire,
                     fInterface = flat },

                   fUniformBuffers = UniformData {
                                       fScalars  = scalars',
                                       fMatrices = matrices',
                                       fVectors  = vectors' },

                   fUniformValues = UniformValues {
                                     fScalars  = [0,0,0],
                                     fMatrices = [identity, perspective (60 * π/180) (1) 1 1000] },

                   fEntities  = [text, minecraft, gourd],

                   fSelection = SelectionData {
                     fSilhouettes  = contours,
                     fDepthTexture = depths } }
