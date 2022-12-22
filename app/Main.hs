module Main (main) where

import Graphics.Image as I
import Lib (fromRGB2D, pixelizeWithR, toGrayScale2D, toTrueGrayScale2D)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let argsNum = length args

  let (sourcePath, resPath, radius) =
        case argsNum of
          3 -> (args !! 0, args !! 1, read $ args !! 2)
          _ -> error "=== Too few arguments. Usage: path to the source file, destination folder and pixelization radius ==="

  l <- readImageExact PNG sourcePath :: IO (Either String (I.Image VS I.RGBA Word8))
  r <- readImageExact PNG sourcePath :: IO (Either String (I.Image VS I.RGB Word8))
  let image = case l of
        Right img -> Left img
        Left _ -> case r of
          Right img -> Right img
          Left _ -> error "Unsupportable format. You can use PNG images only"

  let vec = case image of
        Right im -> fromRGB2D $ toLists im
        Left im -> toLists im
  let grayVec = toGrayScale2D vec
  let pixelized1 = pixelizeWithR vec radius
  let pixelized2 = pixelizeWithR grayVec radius
  let backToImage = fromLists pixelized1 :: I.Image VS I.RGBA Word8
  let backToImageGray = fromLists $ toTrueGrayScale2D pixelized2 :: I.Image VS I.Y Word8
  writeImageExact PNG [] (resPath ++ "/resColored.png") backToImage
  writeImageExact PNG [] (resPath ++ "/resB&W.png") backToImageGray
  putStrLn ("=== Your pixelized images are" ++ " resColored.png and resB&W.png (in black-and-white) " ++ "at " ++ resPath ++ " ===")

--putStrLn "=============================================="
--setDisplayProgram ("gpicview", True)
--displayImage backToImage
