{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import System.Environment (getArgs)
import qualified Codec.Picture.Types as M

import System.Environment( getArgs )
import Codec.Picture.Types

import Debug.Trace
import GHC.Word

paletteList = [
  [
    PixelRGB8 7 5 5,
    PixelRGB8 33 25 25,
    PixelRGB8 82 58 42,
    PixelRGB8 138 107 62,
    PixelRGB8 193 156 77,
    PixelRGB8 234 219 116,
    PixelRGB8 160 179 53,
    PixelRGB8 83 124 68,
    PixelRGB8 66 60 86,
    PixelRGB8 89 111 175,
    PixelRGB8 107 185 182,
    PixelRGB8 251 250 249,
    PixelRGB8 184 170 176,
    PixelRGB8 121 112 126,
    PixelRGB8 148 91 40
  ],
  [
    PixelRGB8 13 43 69,
    PixelRGB8 32 60 86,
    PixelRGB8 84 78 104,
    PixelRGB8 141 105 122,
    PixelRGB8 208 129 89,
    PixelRGB8 255 170 94,
    PixelRGB8 255 212 163,
    PixelRGB8 255 236 214
  ],
  [
    PixelRGB8 43 15 84,
    PixelRGB8 171 31 101,
    PixelRGB8 255 79 105,
    PixelRGB8 255 247 248,
    PixelRGB8 255 129 66,
    PixelRGB8 255 218 69,
    PixelRGB8 51 104 220,
    PixelRGB8 73 231 236
  ],
  [
    PixelRGB8 48 0 48,
    PixelRGB8 96 40 120,
    PixelRGB8 248 144 32,
    PixelRGB8 248 240 136
  ],
  [
    PixelRGB8 239 26 26,
    PixelRGB8 172 23 23,
    PixelRGB8 243 216 216,
    PixelRGB8 177 139 139,
    PixelRGB8 53 52 65,
    PixelRGB8 27 26 29
  ],
  [
    PixelRGB8 26 28 44,
    PixelRGB8 93 39 93,
    PixelRGB8 177 62 83,
    PixelRGB8 239 125 87,
    PixelRGB8 255 205 117,
    PixelRGB8 167 240 112,
    PixelRGB8 56 183 100,
    PixelRGB8 37 113 121,
    PixelRGB8 41 54 111,
    PixelRGB8 59 93 201,
    PixelRGB8 65 166 246,
    PixelRGB8 115 239 247,
    PixelRGB8 244 244 244,
    PixelRGB8 148 176 194,
    PixelRGB8 86 108 134,
    PixelRGB8 51 60 87
  ],
  [
    PixelRGB8 44 33 55,
    PixelRGB8 118 68 98,
    PixelRGB8 237 180 161,
    PixelRGB8 169 104 104
  ],
  [
    PixelRGB8 171 97 135,
    PixelRGB8 235 198 134,
    PixelRGB8 216 232 230,
    PixelRGB8 101 219 115,
    PixelRGB8 112 157 207,
    PixelRGB8 90 104 125,
    PixelRGB8 33 30 51
  ],
  [
    PixelRGB8 140 143 174,
    PixelRGB8 88 69 99,
    PixelRGB8 62 33 55,
    PixelRGB8 154 99 72,
    PixelRGB8 215 155 125,
    PixelRGB8 245 237 186,
    PixelRGB8 192 199 65,
    PixelRGB8 100 125 52,
    PixelRGB8 228 148 58,
    PixelRGB8 157 48 59,
    PixelRGB8 210 100 113,
    PixelRGB8 112 55 127,
    PixelRGB8 126 196 193,
    PixelRGB8 52 133 157,
    PixelRGB8 23 67 75,
    PixelRGB8 31 14 28
  ],
  [
    PixelRGB8 94 96 110,
    PixelRGB8 34 52 209,
    PixelRGB8 12 126 69,
    PixelRGB8 68 170 204,
    PixelRGB8 138 54 34,
    PixelRGB8 235 138 96,
    PixelRGB8 0 0 0,
    PixelRGB8 92 46 120,
    PixelRGB8 226 61 105,
    PixelRGB8 170 92 61,
    PixelRGB8 255 217 63,
    PixelRGB8 181 181 181,
    PixelRGB8 255 255 255
  ],
  [
    PixelRGB8 49 31 95,
    PixelRGB8 22 135 167,
    PixelRGB8 31 213 188,
    PixelRGB8 237 255 177
  ],
  [
    PixelRGB8 21 25 26,
    PixelRGB8 138 76 88,
    PixelRGB8 217 98 117,
    PixelRGB8 230 184 193,
    PixelRGB8 69 107 115,
    PixelRGB8 75 151 166,
    PixelRGB8 165 189 194,
    PixelRGB8 255 245 247
  ]]


converterPlain :: PixelRGB8 -> PixelRGB8
converterPlain p = p

converterGrey :: PixelRGB8 -> PixelRGB8
converterGrey (PixelRGB8 r g b) = 
  let m = maximum [r, g, b] in
  PixelRGB8 m m m


converterPalette :: Int -> PixelRGB8 -> PixelRGB8
converterPalette palette a = foldl (paletteFold a) (PixelRGB8 0 0 0) (paletteList !! palette)
  where
    paletteFold :: PixelRGB8 -> PixelRGB8 -> PixelRGB8 -> PixelRGB8
    paletteFold base x y = if dist base x > dist base y then y else x

    dist :: PixelRGB8 -> PixelRGB8 -> Integer
    dist (PixelRGB8 rx gx bx) (PixelRGB8 ry gy by) = (square rx ry) + (square gx gy) + (square bx by)
    
    square :: Word8 -> Word8 -> Integer
    square a b = (toInteger a - toInteger b)^2


{- convertPixel :: Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convertPixel 1 img x y = grey (pixelAt img x y)
convertPixel 2 img x y = converterPalette 3 (pixelAt img x y)
convertPixel _ img x y = pixelAt img x y -}

divBlock :: Int -> Int -> Int -> Bool
divBlock block x y = (x `mod` block == 0) && (y `mod` block == 0)

nextBlockX :: Int -> Int -> Int -> Int -> Int -> Int
nextBlockX x y block w h
  | divBlock block (x + 1) (y + 1) = if (x + 1 >= w && y + 1 < h) then 0 else x + 1 -- to the next block
  | ((x + 1) `mod` block == 0) = x + 1 - block
  | otherwise = x + 1

nextBlockY :: Int -> Int -> Int -> Int -> Int
nextBlockY x y block w
  | divBlock block (x + 1) (y + 1) = if (x + 1 >= w) then y + 1 else y + 1 - block --to the next block
  | (x + 1) `mod` block == 0 = y + 1
  | otherwise = y

transformRGBImage :: Int -> (PixelRGB8 -> PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
transformRGBImage block converter img@Image {..} = runST $ do
  mimg <- M.newMutableImage imageWidth imageHeight
  let go x y pixel
        | x >= imageWidth && y >= imageHeight = M.unsafeFreezeImage mimg
        | x >= imageWidth || y >= imageHeight = go (nextBlockX x y block imageWidth imageHeight) (nextBlockY x y block imageWidth) pixel
        | otherwise = do
          let nextPixel | divBlock block x y = converter (pixelAt img x y)
                        | otherwise = pixel
          writePixel mimg x y nextPixel
          go (nextBlockX x y block imageWidth imageHeight) (nextBlockY x y block imageWidth) nextPixel
  go 0 0 (PixelRGB8 0 0 0)


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
                          . transformRGBImage block converterGrey $ convertImage img
              --

            Right _ -> putStrLn "Unhandled image colorspace"

