module Main (main) where
import Graphics.Image as I
import Lib (pixelizeWithR, fromRGB2D, toGrayScale2D)
import System.Environment(getArgs)



main :: IO ()
main = do
    args <- getArgs
    --mapM_ putStrLn args
    let argsNum = length args

    let (sourcePath, resPath, radius) =
                case argsNum of
                        3 -> (args!!0, args!!1, read $ args!!2)
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
    -- let grayVec = toGrayScale2D
    let backToImage = fromLists $ pixelizeWithR vec radius :: I.Image VS I.RGBA Word8
    writeImageExact PNG [] (resPath ++ "/res.png") backToImage
    putStrLn ("=== Your pixelized image is" ++ " res.png " ++ "at " ++ resPath ++ "===")
    --putStrLn "=============================================="
    --setDisplayProgram ("gpicview", True)
    --displayImage backToImage
