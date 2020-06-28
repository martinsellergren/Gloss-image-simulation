module World (World, initWorld, renderWorld, stepWorld, handler) where

import Codec.Picture (Image, PixelRGBA8)
import Objects
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort (ViewPort(..), applyViewPortToPicture)

{- World
   REPRESENTATION: A world with objects os, view port vs and pause flag p is given by: W os vp p
   INVARIANT: True
 -}
data World = W Objects ViewPort Bool

{- initWorld ww wh img shape texture objectsOnWidth objectSizeFactor animationOfWindow seedNumber startPositionMode
   PURPOSE: Get initial world.
   PRE: ww, wh andanimationOfWindow >= 0, objectsOnWidth > 0, 0 <= objectSizeFactor <= 1
            (note: objectsOnWidth also determines the size of the objects. If n is low and the picture's ratio
            (width/height) > 1, no objects may fit on the height and therefor no objects will be created.)
   POST: The initial world based on the input arguments.
 -}
initWorld :: Int -> Int -> Image PixelRGBA8 -> Shape -> Texture -> Int -> Float -> Float -> Int -> StartPositionMode -> World
initWorld ww wh img s t n osf aow sn sm =
         W (setStartPositions (initObjects ww wh img s t n osf aow) sn sm) (ViewPort (0,0) 0 1) True

{- renderWorld world
   PURPOSE: Render world.
   PRE: True
   POST: world rendered into picture.
 -}
renderWorld :: World -> Picture
renderWorld (W os vp _) = applyViewPortToPicture vp (renderObjects os)

{- stepWorld aC fC collisions t world
   PURPOSE: Step world one time step.
   PRE: aC, fC and t >= 0. (Note: Keep aC and t small for correct collisions.)
   POST: world after one time step of length t seconds, where acceleration and friction is determined by aC and fC,
         with object collisions if collisions is True. Step not taken if pause flag of world is True.
 -}
stepWorld :: Float -> Float -> Bool -> Float -> World -> World
stepWorld aC fC collisions t w@(W os vp p) = if p
                                                then w
                                                else W (stepObjects t os aC fC collisions) vp p

{-handler event world
   PURPOSE: Handle events.
   PRE: True
   POST: world with changes determined by event. Pause flag and viewport (zoom and rotation) of world is the changeable attributes.
 -}
handler :: Event -> World -> World
handler (EventKey (Char 'p') Up _ _) (W os vp p) = W os vp (not p)
handler (EventKey (Char 'x') Up _ _) (W os vp p) = W os (vp { viewPortScale = (viewPortScale vp)+0.1 }) p
handler (EventKey (Char 'z') Up _ _) (W os vp p) = W os (vp { viewPortScale = (viewPortScale vp)-0.1 }) p
handler (EventKey (Char 's') Up _ _) (W os vp p) = W os (vp { viewPortRotate = (viewPortRotate vp)+2 }) p
handler (EventKey (Char 'a') Up _ _) (W os vp p) = W os (vp { viewPortRotate = (viewPortRotate vp)-2 }) p
handler (EventKey (Char 'r') Up _ _) (W os vp p) = W os (ViewPort (0,0) 0 1) p
handler _ w = w