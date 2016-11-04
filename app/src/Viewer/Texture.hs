-- |
-- Module      : Viewer.Texture
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
module Viewer.Texture where



--------------------------------------------------------------------------------------------------------------------------------------------
-- We'll need these
--------------------------------------------------------------------------------------------------------------------------------------------
import Data.Foldable (toList)

import qualified Codec.Picture       as Juicy
import qualified Codec.Picture.Types as Juicy

import           Graphics.GPipe
import qualified Graphics.GPipe.Context.GLFW        as Context
import           Graphics.GPipe.Context.GLFW.Unsafe (GLFWWindow (..))

import Leibniz (deg, rad, π)



--------------------------------------------------------------------------------------------------------------------------------------------
-- Definitions
--------------------------------------------------------------------------------------------------------------------------------------------

-- TODO: Don't hard-code pixel type

-- |
pixel :: Juicy.ColorSpaceConvertible c Juicy.PixelRGB8 => [V3 Juicy.Pixel8] -> a -> b -> c -> [V3 Juicy.Pixel8]
pixel xs _ _ pix = let Juicy.PixelRGB8 r g b = Juicy.convertPixel pix in V3 r g b : xs


-- |
-- saves :: FilePath -> Texture2D os (HostFormat (BufferColor (Color a (ColorElement a)) a)) -> (a -> V3 Float) -> AppT os ()
save :: FilePath -> Texture2D os (Format RGBFloat) -> (V3 Float -> V3 Float) -> AppT os ()
save fn tex f = do
  pixels <- readTexture2D tex 0 (V2 0 0) size (\ps c -> return $ convert (f c) ++ ps) []
  liftIO $ Juicy.savePngImage fn (Juicy.ImageRGB8 $ image size pixels)
  where
    (size:_) = texture2DSizes tex
    image (V2 dx dy) pixels = Juicy.Image { Juicy.imageWidth = dx, Juicy.imageHeight = dy, Juicy.imageData = VS.fromList pixels }
    pixel8    = floor . (*255)
    convert c = let (V3 r g b) = fmap pixel8 c in [r, g, b]


-- |
-- TODO: Clean this up
load :: FilePath -> AppT os (Either String (Texture2D os (Format RGBFloat)))
load fn = runEitherT $ do
  (Juicy.ImageRGB8 image) <- EitherT (liftIO $ Juicy.readImage fn)
  let size = V2 (Juicy.imageWidth image) (Juicy.imageHeight image)
  tex <- lift $ textureFromPixels size (Juicy.pixelFold pixel [] image)
  return tex


-- |
new :: V2 Int -> (Int -> Int -> V3 Juicy.Pixel8) -> AppT os (Texture2D os (Format RGBFloat))
new size@(V2 dx dy) f = textureFromPixels size [f x y | x <- [0..dx-1], y <- [0..dy-1]]


-- |
-- TODO: Rename (?)
pixelAt :: V2 Int -> Texture2D os (Format RGBFloat) -> ContextT GLFWWindow os (ContextFormat RGBFloat a) IO (Maybe (V3 Float))
pixelAt from tex = do
  mpixels <- readPixels from (V2 1 1) tex
  return $ mpixels >>= listToMaybe


-- |
-- TODO: Figure out order and format
-- TODO: Make polymorphic
readPixels :: V2 Int -> V2 Int -> Texture2D os (Format RGBFloat) -> ContextT GLFWWindow os (ContextFormat RGBFloat a) IO (Maybe [V3 Float])
readPixels from size tex
  | fits (from+size) wholesize = fmap Just $ readTexture2D tex level from size (\ps c -> return (c:ps)) []
  | otherwise = return Nothing
  where
    wholesize = listToMaybe $ texture2DSizes tex
    fits (_)        (Nothing)           = False
    fits (V2 dx dy) (Just (V2 dx' dy')) = (dx <= dx') && (dy < dy')
    level = 0


-- | Creates a texture from a 'Foldable' of pixels
-- TODO: Make sure the size is correct
-- TODO: Figure out what the layout is (eg. col-major, row-major, etc.)
fromPixels :: Foldable t => V2 Int -> t (V3 Juicy.Pixel8) -> AppT os (Texture2D os (Format RGBFloat))
fromPixels size  pixels = do
  -- TODO: What the hell is 'maxBound' doing here (?)
  tex <- newTexture2D SRGB8 size maxBound -- JPG converts to SRGB
  writeTexture2D tex 0 0 size (toList pixels)
  generateTexture2DMipmap tex
  return tex


-- | Creates a monochrome texture
monochrome :: V2 Int -> V3 Juicy.Pixel8 -> AppT os (Texture2D os (Format RGBFloat))
monochrome size@(V2 dx dy) colour = textureFromPixels size (replicate (dx*dy) colour)
