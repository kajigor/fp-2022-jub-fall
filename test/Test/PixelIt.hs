module Test.PixelIt where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import PixelIt
import Codec.Picture.Types

unit_divBlock = do
    divBlock 5 15 20 @?= True
    divBlock 3 10 12 @?= False
    divBlock 100 100 250 @?= False
    divBlock 1 31 43 @?= True


unit_nextBlockX = do
    let w = 100
    let h = 200

    let b = 10
    nextBlockX 0 0 b w h @?= 1
    nextBlockX 9 0 b w h @?= 0
    nextBlockX 9 9 b w h @?= 10
    nextBlockX 99 129 b w h @?= 0
    
    let a = 15
    nextBlockX 0 0 a w h @?= 1
    nextBlockX 14 0 a w h @?= 0
    nextBlockX 29 29 a w h @?= 30
    nextBlockX 99 129 a w h @?= 100
    nextBlockX 104 129 a w h @?= 90
    nextBlockX 104 134 a w h @?= 0


unit_nextBlockY = do
    let w = 100
    -- let h = 200

    let b = 10
    nextBlockY 0 0 b w @?= 0
    nextBlockY 9 0 b w @?= 1
    nextBlockY 9 9 b w @?= 0
    nextBlockY 99 129 b w @?= 130
    
    let a = 15
    nextBlockY 0 0 a w @?= 0
    nextBlockY 14 0 a w @?= 1
    nextBlockY 29 29 a w @?= 15
    nextBlockY 99 129 a w @?= 129
    nextBlockY 104 129 a w @?= 130
    nextBlockY 104 134 a w @?= 135


black :: PixelRGB8
black = PixelRGB8 0 0 0

pixel1 :: PixelRGB8
pixel1 = PixelRGB8 123 32 12

pixel2 :: PixelRGB8
pixel2 = PixelRGB8 145 27 2

unit_converterPlain = do
    converterPlain black @?= black
    converterPlain pixel1 @?= pixel1

unit_converterGrey = do
    converterGrey black @?= black
    converterGrey pixel1 @?= (PixelRGB8 123 123 123)
    converterGrey pixel2 @?= (PixelRGB8 145 145 145)
    converterGrey (PixelRGB8 3 255 43) @?= (PixelRGB8 255 255 255)
    converterGrey (PixelRGB8 10 20 30) @?= (PixelRGB8 30 30 30)

unit_converterIndexPalette = do
    converterIndexPalette 0 (paletteList !! 0 !! 0) @?= (paletteList !! 0 !! 0)
    converterIndexPalette 2 (paletteList !! 2 !! 5) @?= (paletteList !! 2 !! 5)
    converterIndexPalette 4 (paletteList !! 4 !! 3) @?= (paletteList !! 4 !! 3)
    converterIndexPalette 6 (paletteList !! 6 !! 1) @?= (paletteList !! 6 !! 1)
    converterIndexPalette 11 (paletteList !! 11 !! 2) @?= (paletteList !! 11 !! 2)