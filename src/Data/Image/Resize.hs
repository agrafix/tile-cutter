module Data.Image.Resize where

import Codec.Picture

import Control.Arrow ((***), (&&&))

-- | Resize an image by a factor
resizeByFactor ::
    ((Int, Int) -> Image px -> Image px)
    -> Double
    -> Image px
    -> Image px
resizeByFactor resizer factor imageIn =
    let origDims = (imageWidth &&& imageHeight) imageIn
        scaleDims dim = floor $ fromIntegral dim * factor
        newDims = (scaleDims *** scaleDims) origDims
    in if newDims == origDims
       then imageIn
       else resizer newDims imageIn

-- | Resize an image using a simple nearest neighbor algorithm
resizeNearestNeighbor :: Pixel px => (Int, Int) -> Image px -> Image px
resizeNearestNeighbor (newW, newH) imageIn =
    generateImage gen newW newH
    where
      xRatio :: Double
      xRatio = fromIntegral oldW / fromIntegral newW

      yRatio :: Double
      yRatio = fromIntegral oldH / fromIntegral newH
      gen x y =
          let px = floor (fromIntegral x * xRatio)
              py = floor (fromIntegral y * yRatio)
          in pixelAt imageIn px py
      oldW = imageWidth imageIn
      oldH = imageHeight imageIn
