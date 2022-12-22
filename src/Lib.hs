module Lib (pixelizeWithR, fromRGB2D, pixListAveragePixel, pixelizeN, toGrayScale2D, toTrueGrayScale2D) where

import Data.List (transpose)
import Data.List.Split
import Data.Ratio
import Graphics.Image as I
import Prelude as P

type Pixel8 = Word8

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

fromRGB1D :: [Pixel RGB Word8] -> [Pixel RGBA Word8]
fromRGB1D [] = []
fromRGB1D ((PixelRGB r g b) : xs) = PixelRGBA r g b 255 : fromRGB1D xs

fromRGB2D :: [[Pixel RGB Word8]] -> [[Pixel RGBA Word8]]
fromRGB2D = P.map fromRGB1D

pixelToGrayScale :: Pixel RGBA Word8 -> Pixel RGBA Word8
pixelToGrayScale (PixelRGBA r g b _) = PixelRGBA (round (pr + pg + pb)) 0 0 255
  where
    pr = toRational r * (299 % 1000)
    pg = toRational g * (587 % 1000)
    pb = toRational b * (114 % 1000)

toGrayScale1D :: [Pixel RGBA Word8] -> [Pixel RGBA Word8]
toGrayScale1D = P.map pixelToGrayScale

toGrayScale2D :: [[Pixel RGBA Word8]] -> [[Pixel RGBA Word8]]
toGrayScale2D = P.map toGrayScale1D

rGBAToTrueGray :: Pixel RGBA Word8 -> Pixel Y Word8
rGBAToTrueGray (PixelRGBA r _ _ _) = fromIntegral r

toTrueGrayScale1D :: [Pixel RGBA Word8] -> [Pixel Y Word8]
toTrueGrayScale1D = P.map rGBAToTrueGray

toTrueGrayScale2D :: [[Pixel RGBA Word8]] -> [[Pixel Y Word8]]
toTrueGrayScale2D = P.map toTrueGrayScale1D
