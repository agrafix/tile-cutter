{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Image.Resize
import Data.Image.TileCutter

import Codec.Picture
import Control.Monad
import Options.Applicative
import System.Directory
import System.FilePath

data Opts
   = Opts
   { o_sourceFile :: !FilePath
   , o_resizeFactor :: !Double
   , o_tileSize :: !Int
   , o_outDir :: !FilePath
   } deriving (Show, Eq)

optsP :: Parser Opts
optsP =
    Opts
    <$> strOption ( long "source-file" <> metavar "FILE" <> help "File to read" )
    <*> option auto ( long "resize-factor" <> metavar "DOUBLE" <> help "Resize image" )
    <*> option auto ( long "tile-size" <> metavar "PIXEL" <> help "Square tile size in pixels")
    <*> strOption ( long "out-dir" <> metavar "DIR" <> help "File to write generated tiles to" )

main :: IO ()
main =
    execParser allOpts >>= \opts ->
    do createDirectoryIfMissing True (o_outDir opts)
       imageDyn <-
           readImage (o_sourceFile opts) >>=
           \case
             Left errMsg -> fail errMsg
             Right ok -> pure ok
       let image = convertRGBA8 imageDyn
           imageResized =
               resizeByFactor resizeNearestNeighbor (o_resizeFactor opts) image
           tiles =
               cutTiles (PixelRGBA8 0 0 0 0) (o_tileSize opts) imageResized
       writePng (o_outDir opts </> "resized.png") imageResized
       forM_ (zip tiles [0..]) $ \(tileRow, (rowIdx :: Int)) ->
           forM_ (zip tileRow [0..]) $ \(tileCol, (colIdx :: Int)) ->
           writePng (o_outDir opts </> "tile_" ++ show rowIdx ++ "_" ++ show colIdx ++ ".png") tileCol
    where
      allOpts =
          info (helper <*> optsP)
          ( fullDesc
          <> progDesc "TileCutter"
          <> header "Cut square tiles from a given input image" )
