import Graphics.Gloss
import System.Environment
import Codec.Picture (Image, readImage, convertRGBA8, PixelRGBA8)
import System.Random (randomIO)
import Objects (Shape(..), Texture(..), StartPositionMode(..))
import World
import TestCases
import Test.HUnit

window = FullScreen (ww,wh)--InWindow "animation" (ww,wh) (0,0) --the basic window-properties.
fps = 50 --frames per seconds
ww = 1280 --window width
wh = 768 --window height
animationOfWindow = 0.6 --the amount of screen wdith/height the completed animation takes up. Width or height determined by image dimensions.

{- input arguments example
imgPath = "foto.bmp"
objectSizeFactor = 0.7
objectMode = 3
objectsOnWidth = 10
startMode = RandomSquare
collisions = True
accConstant = 2
frictionConstant = 100
-}

{- main
   PURPOSE: Start the simulation.
   SIDE EFFECTS:  Reads input arguments:
                     - imgPath - path to image to use in simulation.
                     - objectsOnWidth - numer of objects on one row to split the image into, > 0
                     - objectMode - 1 to 4, to specify object shape and texture
                     - startMode - start position mode for the objects, RandomSquare or Done
                     - accConstant - determines how fast an object's acceleration increases with distance from its target. Recommended: 0.1 - 2
                     - frictionConstant - amount of friction, i.e the norm of the friction vector for the objects. Recommended: 100
                           (note: increase frictionConstant AND accConstant to make simulation complete faster.)
                     - collisions - True or False whether objects should collide.
                     - objectSizeFactor - A value between 0 and 1 where 1 means no padding around objects and 0 means no size
                  Reads image from hard drive.
                  Outputs graphics based on input.
 -}
main :: IO ()
main = do
         [imgPath, objectsOnWidth', objectMode, startMode', accConstant', frictionConstant', collisions', objectSizeFactor'] <- getArgs
         let (objectsOnWidth, startMode, accConstant, frictionConstant, collisions, objectSizeFactor) =
               (read objectsOnWidth'::Int, read startMode'::StartPositionMode, read accConstant'::Float,
               read frictionConstant'::Float, read collisions'::Bool, read objectSizeFactor'::Float)
         let (shape,texture) = case objectMode of
                                 "1" -> (Ball,Solid)
                                 "2" -> (Square,Solid)
                                 "3" -> (Ball,Vivid)
                                 "4" -> (Square,Vivid)
                                 _ -> error "wrong input: object mode"
         img <- getImage imgPath
         seedNumber <- randomIO :: IO Int
         let init = initWorld ww wh img shape texture objectsOnWidth objectSizeFactor animationOfWindow seedNumber startMode
         play window black fps init renderWorld handler (stepWorld accConstant frictionConstant collisions)


{- getImage path
   PURPOSE: Load image from hard drive and convert it.
   PRE: Path is a valid path (on the form a/b/c.jpg) to and image in a common format.
   POST: Image specified by img.
   SIDE EFFECTS: Reads image from hard drive.
   EXAMPLES:   Valid path and successful read: getImage "duck.bmp" = the image
               Invalid path or other error: terminates program with the error text "error loading image"
 -}
getImage :: String -> IO (Image PixelRGBA8)
getImage imgPath = do
                     dynImg <- readImage imgPath
                     case dynImg of
                        Left _ -> error "error loading image"
                        Right dynImg ->
                           return $ convertRGBA8 dynImg