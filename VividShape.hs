module VividShape where

import Codec.Picture (Image(..), PixelRGBA8(..), pixelAt, generateImage)
import Graphics.Gloss
import Graphics.Gloss.Juicy (fromImageRGBA8)
import Graphics.Gloss.Data.ViewPort (ViewPort(..), applyViewPortToPicture)
import Test.HUnit


{- createVividPic d img ibx iby ibd f
   PURPOSE: Transform a box inside an image into a gloss picture, with potentially large differences after the transformation.
   PRE: ibx + ibd <= img width, iby + ibd <= img height. ibx, iby >= 0. d and ibd > 0
         f a function that takes two arguments a1 a2, gives a pixel, and provieds valid results for all a1<-[0..ibd-1], a2<-[0..ibd-1].
   POST: Picture with side length d, vivid color by image box positioned at (ibx,iby) with side length ibd in img.
         The pixels in the picture is the result of calling f with every a1 a2 belonging to [0..ibd-1]
 -}
createVividPic :: Float -> Image PixelRGBA8 -> Int -> Int -> Int -> (Int -> Int -> PixelRGBA8) -> Picture
createVividPic d img ibx iby ibd f =   let
                                          img'@(Image w' h' _) = generateImage f ibd ibd
                                          vp = ViewPort (0,0) 0 (d / fromIntegral ibd)
                                       in
                                          applyViewPortToPicture vp (fromImageRGBA8 img')

{- makeBall img bx by bd x y
   PURPOSE: Get pixel in image where absolute pixel position determined by offset and relativ position,
            and make it transparent if outside circle.
   PRE: x <= bd, y <= bd. (bx+bd, by+bd) inside img. bx, by, x, y >= 0. bd > 0.
   POST: The pixel at pixel pos=(bx+x, by+y) in img, transparent if outside a circle that fits
         tightly inside a box with top left corner at (bx,by) and dim bd*bd.
 -}
makeBall :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> Int -> PixelRGBA8
makeBall img bx by bd x y =
               let
                  bxc = fromIntegral bd / 2  --image box relative center
                  byc = fromIntegral bd / 2  --image box relative center
                  d = sqrt ((fromIntegral x - bxc)^2 + (fromIntegral y - byc)^2)
               in
                  if d <= fromIntegral bd / 2
                     then pixelAt img (bx+x) (by+y)
                     else PixelRGBA8 0 0 0 0

{- makeSquare img bx by x y
   PURPOSE: Get pixel in image where absolute pixel position determined by offset and relativ position.
   PRE: (bx+x, by+y) inside img. bx, by, x, y >= 0.
   POST: The pixel at pixel pos=(bx+x, by+y) in img.
 -}
makeSquare :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
makeSquare img bx by x y = pixelAt img (bx+x) (by+y)

{- adjustForVividShape ibx iby ibd osf
   PURPOSE: Make adjustments to position and dimension suitable for creating vivid shapes.
   PRE: ibx, iby >= 0. ibd, osf > 0.
   POST: Image box posision and side length adjusted according to object size factor osf: (ibx',iby',ibd')
 -}
adjustForVividShape :: Int -> Int -> Int -> Float -> (Int,Int,Int)
adjustForVividShape ibx iby ibd osf =  let
                                          ibd' = floor (fromIntegral ibd*osf)
                                          ibx' = ibx + ((ibd-ibd') `div` 2)
                                          iby' = iby + ((ibd-ibd') `div` 2)
                                       in
                                          (ibx', iby', ibd')

--TEST CASES-----------------------------------------------------------------------------------------------------------------------------------------------------
testAdjustForVividShape = TestCase (assertEqual "adjustForVividShape 30 30 450 0" (255,255,0) (adjustForVividShape 30 30 450 0))

vividShapeTest = TestList [testAdjustForVividShape]