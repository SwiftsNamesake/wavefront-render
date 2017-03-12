-- |
-- Module      : Viewer.Mesh
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
-- API
--------------------------------------------------------------------------------------------------------------------------------------------
module Viewer.Mesh where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Linear hiding (perspective)
import Graphics.GPipe

import Leibniz (deg, rad, π)

import           Viewer.Types
import qualified Viewer.Shader as Shader
import           Viewer.Trinkets



--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- Render ----------------------------------------------------------------------------------------------------------------------------------

-- |
renderSolid :: App os p -> Mesh os Triangles -> Render os f a
renderSolid app mesh = do
  vertexArray <- newVertexArray (mesh^.vertices)
  (app^.solid) $ Shader.makeShaderData


-- |
-- TODO: Scaling options
renderWire :: App os p -> Mesh os Lines -> Render os f a
renderWire app mesh = do
  vertexArray <- newVertexArray (mesh^.vertices)
  (app^.wire) $ Shader.makeShaderData

-- Create ----------------------------------------------------------------------------------------------------------------------------------

-- TODO | - Do not hard-code vertex format
--        -

-- | Loads an OBJ mesh
-- TODO: Rename (?)
newOBJMesh :: FilePath -> AppT os (Either String (Mesh os Triangles))
newOBJMesh fn = runEitherT $ do
  model <- EitherT $ liftIO (Load.model fn)
  EitherT $ fromOBJModel (takeDirectory fn) model


-- |
newQuadXY :: V2 Int -> (V3 Float -> V2 Float) -> (V3 Float -> V3 Float) -> Texture2D os (Format RGBFloat) -> AppT os (Mesh os Triangles)
newQuadXY size texcoord colour tex = do

  vertexBuffer :: Buffer os VertexAttribute <- newBuffer (length vertices)
  writeBuffer vertexBuffer 0 vertices

  return $ Mesh { fVertices  = vertexBuffer,
                  fPrimitive = TriangleList,
                  fTexture   = tex }
  where
    (V2 dx dy) = fmap fromIntegral size
    makeVertex v = (to4D 1 v, V3 0 0 1, texcoord v, colour v)
    vertices = map makeVertex (concat . triangles $ planeXY V3 dx dy)


-- | Constructs a mesh from an OBJ model
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
    -- TODO: Deal with missing or leftover values
    vertexBuffer :: Buffer os VertexAttribute <- newBuffer (length vs)
    writeBuffer vertexBuffer 0 . toList $ V.zipWith4 makeVertex vs ns ts cs

    return $ Mesh { fVertices  = vertexBuffer,
                    fPrimitive = TriangleList,
                    fTexture   = tex }
  where
    makeVertex v n (V2 tx ty) (Colour r g b _) = (to4D 1 v, n, V2 (1-tx) ty, V3 r g b)

    texpath = root </> "textures/"
    texname = listToMaybe . map T.unpack . toList . textures
