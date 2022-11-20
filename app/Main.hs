module Main where

import Codec.Picture (PixelRGB8 (PixelRGB8))
import Data.List.Split
import Graphics.Image as I
import Prelude as P
import Data.List ( transpose )

pixListAveragePixel :: [Pixel RGBA Word8] -> Pixel RGBA Word8
pixListAveragePixel pixList = averagePixel $ P.map (\x -> x `div` length pixList) (go pixList [0, 0, 0, 0])
  where
    go :: [Pixel RGBA Word8] -> [Int] -> [Int]
    go [] sumList = sumList :: [Int]
    go ((PixelRGBA r g b a) : pixList) (sr : sg : sb : sa : sumList) = go pixList [sr + fromIntegral r, sg + fromIntegral g, sb + fromIntegral b, sa + fromIntegral a]
    go _ _ = undefined

averagePixel :: [Int] -> Pixel RGBA Word8
averagePixel (r : g : b : a : x) = PixelRGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) (fromIntegral a)
averagePixel _ = undefined

pixelizeN :: [Pixel RGBA Word8] -> Int -> [Pixel RGBA Word8]
pixelizeN l n = replicate n (pixListAveragePixel l)

pixelizeTwo :: Pixel RGBA Word8 -> Pixel RGBA Word8 -> Pixel RGBA Word8
pixelizeTwo (PixelRGBA r1 g1 b1 a1) (PixelRGBA r2 g2 b2 a2) = PixelRGBA (r1 `div` 2 + r2 `div` 2) (g1 `div` 2 + g2 `div` 2) (b1 `div` 2 + b2 `div` 2) (a1 `div` 2 + a2 `div` 2)

pixLamb :: [Pixel RGBA Word8] -> [Pixel RGBA Word8]
pixLamb [p1, p2] = [pixelizeTwo p1 p2, pixelizeTwo p1 p2]
pixLamb _ = undefined

pixelizeImg :: [[Pixel RGBA Word8]] -> [[Pixel RGBA Word8]]
pixelizeImg = foldr (\x -> (++) [concatMap pixLamb (chunksOf 2 x)]) []

pixelizeImgN :: [[Pixel RGBA Word8]] -> Int -> [[Pixel RGBA Word8]]
pixelizeImgN l n = foldr (\x -> (++) [concatMap (`pixelizeN` n) (chunksOf n x)]) [] l

pixelizeImgRows :: [[Pixel RGBA Word8]] -> [[Pixel RGBA Word8]]
pixelizeImgRows [] = []
pixelizeImgRows [x] = [x]
pixelizeImgRows (x1 : x2 : xs) = [P.zipWith pixelizeTwo x1 x2] ++ [P.zipWith pixelizeTwo x1 x2] ++ pixelizeImgRows xs

pixelizeWithR :: [[Pixel RGBA Word8]] -> Int -> [[Pixel RGBA Word8]]
pixelizeWithR vec n = Data.List.transpose (pixelizeImgN (Data.List.transpose (pixelizeImgN vec n)) n)

main :: IO ()
main = do
  l <- readImageExact PNG "Lenna.png" :: IO (Either String (I.Image VS I.RGBA Word8))
  let image = case l of
        Right img -> img
        Left a -> error "Wrong format of image"
  let vec = toLists image

  let backToImage = fromLists $  pixelizeWithR vec 140 :: I.Image VS I.RGBA Word8
  writeImageExact PNG [] "res.png" backToImage
  putStrLn "HeHaskell!"
