{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import System.FilePath (replaceExtension)
import qualified Codec.Picture.Types as M

import System.Environment( getArgs )
import Codec.Picture.Types
import Data.Vector
import Data.Vector.Storable

import Debug.Trace
import GHC.Word


{- transformImage :: Image PixelYCbCr8 -> Image PixelYCbCr8
transformImage img = img -- Transform the image here -}


averagePixel :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
averagePixel img x y = pixelAt img x y


getPixel :: Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
getPixel block img x y 
  | (x `mod` block == 0) && (y `mod` block == 0) =  averagePixel img x y
  | otherwise = getPixel block img (prvX x) (prvY x y)
  where
    prvX :: Int -> Int
    prvX x 
      | x `mod` block == 0 = x 
      | otherwise = x - 1

    prvY :: Int -> Int -> Int
    prvY x y
      | x `mod` block == 0 = y - 1
      | otherwise = y


transformRGBImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
transformRGBImage block img@Image {..} = runST $ do
  mimg <- M.newMutableImage imageWidth imageHeight
  let go x y
        | x >= imageWidth = go 0 (y + 1)
        | y >= imageHeight = M.unsafeFreezeImage mimg
        | otherwise = do
            writePixel mimg x y (getPixel block img x y)
            go (x + 1) y
  go 0 0


main :: IO ()
main = do
    commandArguments <- getArgs
    case commandArguments of
      [] -> putStrLn "Not enough arguments"
      (filename : block : _) -> pixelIt filename (read block)
      (filename : _) -> pixelIt filename 10
    where 
        pixelIt :: [Char] -> Int -> IO() 
        pixelIt filename block = do
          dynImg <- readImage filename
          case dynImg of
              Left err -> putStrLn err

              Right (ImageYCbCr8 img) ->
                  writePng (filename Prelude.++ "_transformed.png")
                            . transformRGBImage block $ convertImage img
                --

              Right _ -> putStrLn "Unhandled image colorspace"

