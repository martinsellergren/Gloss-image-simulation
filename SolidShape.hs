module SolidShape (avgBoxColor, solidShapeTest) where

import Graphics.Gloss
import Codec.Picture (Image, PixelRGBA8(..), pixelAt, generateImage)
import Test.HUnit

{- avgBoxColor img x y d
   PURPOSE: Get the average color of a box in an image.
   PRE: img width >= x+d, img height >= y+d. x, y and d >= 0.
   POST: Average color of box in img at pixel location (x,y) with sides d pixels.
 -}
avgBoxColor :: Image PixelRGBA8 -> Int -> Int -> Int -> Color
avgBoxColor img x y d = avgColor [toRGB $ pixelAt img x y | x <- [x..(x+d-1)], y <- [y..(y+d-1)]]

--toRGB pixel
--post: the rgb of the pixel
toRGB :: PixelRGBA8 -> (Float, Float, Float)
toRGB (PixelRGBA8 r g b _) = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255)

--avgColor rgbs
--post: average color of all rgb values in rgbs
avgColor :: [(Float, Float, Float)] -> Color
avgColor cs =  let
                  r = avg $ map (\(r,_,_) -> r) cs
                  g = avg $ map (\(_,g,_) -> g) cs
                  b = avg $ map (\(_,_,b) -> b) cs
               in
                  makeColor r g b 1

--avg xs
--post: average value of elements in list
avg :: [Float] -> Float
avg xs = (sum xs) / (fromIntegral $ length xs)

--TEST CASES-----------------------------------------------------------------------------------------------------------------------------------------------------

--creating sample one color Image
imageCreator = generateImage (pixelRenderer) 10 10
   where pixelRenderer _ _ = PixelRGBA8 255 0 100 1  

testAvgBoxColor = TestCase (assertEqual "avgBoxColor (Image PixelRGBA8 255 0 100 1)" (1.0, 0.0, 0.39215687, 1.0) (rgbaOfColor (avgBoxColor (imageCreator) 5 5 3)))

testToRGB = TestCase (assertEqual "toRGB (PixelRGBA8 255 255 255 1)" (1, 1, 1) (toRGB (PixelRGBA8 255 255 255 1)))

testAvgColor = TestCase (assertEqual "avgColor [(0.0, 0.4, 0.2), (0.0, 0.2, 0.2)]" (0, 0.3, 0.2, 1) (rgbaOfColor (avgColor [(0.0, 0.4, 0.2), (0.0, 0.2, 0.2)])))

testAvg = TestCase (assertEqual "avg [30.4,50.1,12.6,78.4]" 42.875 (avg [30.4,50.1,12.6,78.4]))

--tests to export
solidShapeTest = TestList [testAvgBoxColor, testToRGB, testAvgColor, testAvg]

