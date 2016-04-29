{-# LANGUAGE ScopedTypeVariables #-}
module Data.Image.TileCutter where

import Control.Arrow ((&&&))
import Codec.Picture
import qualified Control.Parallel.Strategies as P

-- | Scale and cut a 'DynamicImage' into equally sized tiles and
-- fill empty space with either black or a clear color
-- depending on color space
cutTilesDyn :: Int -> DynamicImage -> [[DynamicImage]]
cutTilesDyn tileSize dynImg =
    case dynImg of
        ImageY8 im -> app ImageY8 0 im
        ImageY16 im -> app ImageY16 0 im
        ImageYF im -> app ImageYF 0 im
        ImageYA8 im -> app ImageYA8 (PixelYA8 0 0) im
        ImageYA16 im -> app ImageYA16 (PixelYA16 0 0) im
        ImageRGB8 im -> app ImageRGB8 (PixelRGB8 0 0 0) im
        ImageRGB16 im -> app ImageRGB16 (PixelRGB16 0 0 0) im
        ImageRGBF im -> app ImageRGBF (PixelRGBF 0 0 0) im
        ImageRGBA8 im -> app ImageRGBA8 (PixelRGBA8 0 0 0 0) im
        ImageRGBA16 im -> app ImageRGBA16 (PixelRGBA16 0 0 0 0) im
        ImageYCbCr8 im -> app ImageYCbCr8 (PixelYCbCr8 0 0 0) im
        ImageCMYK8 im -> app ImageCMYK8 (PixelCMYK8 0 0 0 0) im
        ImageCMYK16 im -> app ImageCMYK16 (PixelCMYK16 0 0 0 0) im
    where
      app f p i =
          map (map f) $ cutTiles p tileSize i

-- | Cut a picture in equally sized tiles
cutTiles :: forall px. Pixel px => px -> Int -> Image px -> [[Image px]]
cutTiles emptyPixel tileSize imageIn =
    flip (P.parMap st) [0..(tileRows - 1)] $ \rowIdx ->
    flip (P.parMap st) [0..(tileCols - 1)] $ \colIdx ->
    generateImage (genFun colIdx rowIdx) tileSize tileSize
    where
      st = P.rpar
      genFun :: Int -> Int -> Int -> Int -> px
      genFun colIdx rowIdx x y =
          getPixel (x, y) (colIdx, rowIdx)
      translate (tx, ty) (colIdx, rowIdx) =
          let cornerX = tileSize * colIdx
              cornerY = tileSize * rowIdx
          in (cornerX + tx, cornerY + ty)
      getPixel innerCoords tileCoords =
          let (x, y) = translate innerCoords tileCoords
          in if x >= 0 && x < imw && y >= 0 && y < imh
             then pixelAt imageIn x y
             else emptyPixel
      tileCols = ceiling ((fromIntegral imh / fromIntegral tileSize) :: Double)
      tileRows = ceiling ((fromIntegral imw / fromIntegral tileSize) :: Double)
      (imw, imh) = (imageHeight &&& imageWidth) imageIn
