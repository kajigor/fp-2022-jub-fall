{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GADTs #-}

module Main (main) where

import PixelIt
import Codec.Picture
import Data.Maybe

import System.Environment( getArgs )
import Codec.Picture.Types

-- checkBlock :: [Char] -> Int
-- checkBlock input = do
--   let block = read input
--   if block <= 0
--   then 
--     putStrLn "The size of pixelblock is less than one" 
--   else block

getConverter :: Int -> Int -> Bool -> (PixelRGB8 -> PixelRGB8)
getConverter palette grey isPalette
  | grey /= 0 && isPalette = converterPaletteGrey (palette - 1)
  | grey /= 0 = converterGrey
  | isPalette = converterPalette (palette - 1)
  | otherwise = converterPlain 

main :: IO ()
main = do
  args <- getArgs

  let (filename, block, palette, grey) = 
        case length args of
          0 -> error "Not enough arguments"
          1 -> (args !! 0, "10", "0", "0")
          2 -> (args !! 0, args !! 1, "0", "0")
          3 -> (args !! 0, args !! 1, args !! 2, "0")
          _ -> (args !! 0, args !! 1, args !! 2, args !! 3)

  let rblock = read block :: Int 
  let rpalette = read palette :: Int 
  let rgrey = read grey :: Int

  let isPalette = rpalette > 0 && rpalette <= length paletteList 
  
  let converter = getConverter rpalette rgrey isPalette

  pixelIt filename rblock converter
  
  where 
    pixelIt :: [Char] -> Int -> (PixelRGB8 -> PixelRGB8) -> IO() 
    pixelIt filename block converter = do
      dynImg <- readImage filename
      case dynImg of
          Left err -> putStrLn err

          Right (ImageYCbCr8 img) ->
              writePng (filename Prelude.++ "_transformed.png")
                        . transformRGBImage block converter $ convertImage img
            --

          Right _ -> putStrLn "Unhandled image colorspace" 

