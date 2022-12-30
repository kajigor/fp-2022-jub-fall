{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Codec.Picture.Types
import Control.Monad.ST
import System.Environment (getArgs)
import qualified Codec.Picture.Types as P

newProcessedPixel :: Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
newProcessedPixel sz img w h = pixelAt img (w - (w `mod` sz)) (h - (h `mod` sz))

pixelization :: Int -> Image PixelRGB8 -> Image PixelRGB8
pixelization sz img@Image {..} = runST $ do
  image <- P.newMutableImage imageWidth imageHeight
  let process w h
        | w >= imageWidth  = P.unsafeFreezeImage image
        | h >= imageHeight = process (w + 1) 0
        | otherwise = do
            writePixel image w h (newProcessedPixel sz img w h)
            process w (h + 1)
  process 0 0

main :: IO ()
main = do
    args <- getArgs
    let (path, sz) =
            case length args of
              2 -> (args !! 0, read $ args !! 1)
              _ -> error "err. Put path and size of cell"
    readImg <- readImage path
    case readImg of
        Right (ImageYCbCr8 image) ->
            (writePng ("pixeled.png") (pixelization sz (convertImage image)))  -- from jpg to PixelRGB8
        Right _ -> putStrLn "wrong type of image (use jpg -> png)"
        Left err -> putStrLn err



