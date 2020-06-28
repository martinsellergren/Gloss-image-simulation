module Objects (Objects, Shape(..), Texture(..), StartPositionMode(..), initObjects, setStartPositions, renderObjects, stepObjects, objectTest) where

import Graphics.Gloss
import Codec.Picture (Image(..), PixelRGBA8, pixelAt, generateImage)
import System.Random (StdGen, mkStdGen, randomR)
import SolidShape
import VividShape
import Test.HUnit

{- Object
   REPRESENTATION: Object of picture pic, side length(dim) d, position (x,y) speed (vx,vy) and
               target position (tx,ty) is given by O pic d (x,y) (vx,vy) (tx,ty). pic has location (0,0).
   INVARIANT: d >= 0
 -}
data Object = O Picture Float (Float,Float) (Float,Float) (Float,Float) deriving (Show, Eq) 

{- Objects
   REPRESENTATION: List of objects os where every object has same shape s, given by Os s os
   INVARIANT: True
 -}
data Objects = Os Shape [Object]

{- Shape
   REPRESENTATION: Defines the different shapes of an object.
 -}
data Shape = Ball | Square

{- Texture
   REPRESENTATION: Defines the texture of an object, where solid means monochrome and vivid means polychrome.
 -}
data Texture = Solid | Vivid

{- StartPositionMode
   REPRESENTATION: Defines different starting constellations of objects.
                  RandomSquare means randomly positioned in a square, Done means start positions are same as target positions.
 -}
data StartPositionMode = RandomSquare | Done deriving Read

-------------------------------------------------------------------------------------------------------------------------init

{- initObjects ww wh img shape textrue objectsOnWidth objectSizeFactor animationOfWindow
   PURPOSE: Get initial list of objects.
   PRE: ww, wh and animationOfWindow >= 0, objectsOnWidth > 0, 0 <= objectSizeFactor <= 1
   POST: img transformed into objects of specified shape and texture, with dimensions and target positions according
       to objectsOnWidth (=number of objects on screen width), window dim (ww*wh) and objectSizeFactor, positioned at (0,0) and with no speed.
 -}
initObjects :: Int -> Int -> Image PixelRGBA8 -> Shape -> Texture -> Int -> Float -> Float -> Objects
initObjects ww wh img@(Image iw ih _) s t n osf aow =
               let
                  ibd = iw `div` n  --image box dim
                  img'@(Image _ ih' _) = crop img iw (ih - ih `mod` ibd)
                  ibps = [(x,y) | x <- [0,ibd..(iw-ibd)], y <- [0,ibd..(ih'-ibd)]]  --image box positions
                  (aw,ah) = getAnimationDim ww wh iw ih' aow
               in
                  Os s (map (createObject img' s t aw ah osf ibd) ibps)

--crop img width height
--post: new image composed of the top left corner of img, with dimensions width*height
crop :: Image PixelRGBA8 -> Int -> Int -> Image PixelRGBA8
crop img w h = generateImage (\x y -> pixelAt img x y) w h


--createObject img s t aw ah osf ibd (ibx,iby)
--post: Object of shape s and texture t at pos (0,0) with speed (0,0) and with size and target position relative
--         to position (ibx,iby) and size ibd*ibd of img, where size is finally multiplied by object size factor osf.
createObject :: Image PixelRGBA8 -> Shape -> Texture -> Float -> Float -> Float -> Int -> (Int,Int) -> Object
createObject img@(Image iw ih _) s t aw ah osf ibd (ibx,iby) =
               let
                  (x,y,d) = getObjectData aw ah iw ih ibx iby ibd osf
                  (ibx',iby',ibd') = adjustForVividShape ibx iby ibd osf
               in
                  case (s,t) of
                     (Ball, Solid) -> O (Color (avgBoxColor img ibx iby ibd) $ circleSolid (d/2)) d (0,0) (0,0) (x,y)
                     (Square, Solid) -> O (Color (avgBoxColor img ibx iby ibd) $ rectangleSolid d d) d (0,0) (0,0) (x,y)
                     (Ball, Vivid) -> O (createVividPic d img ibx' iby' ibd' (makeBall img ibx' iby' ibd')) d (0,0) (0,0) (x,y)
                     (Square, Vivid) -> O (createVividPic d img ibx' iby' ibd' (makeSquare img ibx' iby')) d (0,0) (0,0) (x,y)


--getObjectData aw ah iw ih ibx iby ibd osf
--post: (x,y,d) which describes an object with center at (x,y) of size d*d. Object is at same relative position in animation area
--         aw*ah as the position (ibx+ibd/2, iby+ibd/2) in img, and with dimension d 'relative to image box dim ibd of img size' multiplied by osf.
getObjectData :: Float -> Float -> Int -> Int -> Int -> Int -> Int -> Float -> (Float,Float,Float)
getObjectData aw ah iw ih ibx iby ibd osf =
               let
                  ibd' = fromIntegral ibd
                  iw' = fromIntegral iw
                  ih' = fromIntegral ih
                  ibx' = fromIntegral ibx + ibd'/2   --image box center
                  iby' = fromIntegral iby + ibd'/2   --image box center
                  icx = iw' / 2    --image center
                  icy = ih' / 2   --image center
                  x = ((ibx' - icx) / icx) * (aw / 2)
                  y = ((icy - iby') / icy) * (ah / 2)
                  d = (ibd' / iw') * aw * osf
               in
                  (x,y,d)

 
--getAnimationDim ww wh iw ih aow
--post: (animation width, animation height), based on image dim (iw*ih), window dim (ww*wh) and animationOfWindiw aow,
--          so that the finished animation picture takes up animationOfWindiw % of window width or height (depending on ratio)
getAnimationDim :: Int -> Int -> Int -> Int -> Float -> (Float, Float)
getAnimationDim ww wh iw ih aow =   let
                                       iw' = fromIntegral iw
                                       ih' = fromIntegral ih
                                       ww' = fromIntegral ww
                                       wh' = fromIntegral wh
                                    in
                                       if ih' / iw' > wh' / ww'
                                          then  let
                                                   ah = aow * wh'
                                                   aw = ah * (iw'/ih')
                                                in
                                                   (aw, ah)
                                          else  let
                                                   aw = aow * ww'
                                                   ah = aw * (ih'/iw')
                                                in
                                                   (aw, ah)
                                                   
-------------------------------------------------------------------------------------------------------------------------start pos

{- setStartPositions os sn mode
   PURPOSE: Set start position for objects.
   PRE: True
   POST: Each object in os set to start position according to start position mode, with possible help of seed number sn.
 -}
setStartPositions :: Objects -> Int -> StartPositionMode -> Objects
setStartPositions (Os s os) sn Done = let
                                          setPos :: Object -> Object
                                          setPos (O p d _ v tp) = O p d tp v tp
                                       in
                                          Os s $ map setPos os
setStartPositions (Os s os) sn RandomSquare =   let
                                                   poss = map (\(O _ _ _ _ (x,y)) -> (x,y)) os
                                                in
                                                   Os s $ randomizeStartPositions os poss (mkStdGen sn)

--randomizeStartPositions os poss gen
--post: Each object in os assigned a position in poss based on random assignment by generator gen.
randomizeStartPositions :: [Object] -> [(Float,Float)] -> StdGen -> [Object]
randomizeStartPositions [] _ _ = []
randomizeStartPositions ((O pic d _ v tp):os) ps g = let
                                                         (p,ps',g') = randomElement ps g
                                                      in
                                                         O pic d p v tp : randomizeStartPositions os ps' g'

--randomElement xs g
--post: (x,xs',g') where x is a random element in xs, xs' is xs without x and g' is a fresh generator
randomElement :: [a] -> StdGen -> (a, [a], StdGen)
randomElement xs g = let
                        (i,g') = randomR (0, length xs-1) g
                        xs' = take i xs ++ drop (i+1) xs
                     in
                        (xs !! i, xs', g')

-------------------------------------------------------------------------------------------------------------------------render

{- renderObjects os
   PURPOSE: Render objects.
   PRE: True
   POST: Picture containing every object's picture in os, where each objects picture is positioned according to its position (x,y).
 -}
renderObjects :: Objects -> Picture
renderObjects (Os _ os) = Pictures $ map renderObject os

renderObject :: Object -> Picture
renderObject (O pic _ (x,y) _ _) = Translate x y $ pic

-------------------------------------------------------------------------------------------------------------------------step

{- stepObject t os aC fC collisions
   PURPOSE: Step objects one time step.
   PRE: aC, fC and t >= 0
   POST: The position and velocity of each object in os updated based on amount of time that passed since last step i.e t seconds.
         New position is equal to old position plus old velocity multiplied by t.
         New velocity is equal to old velocity plus acceleration which is a function of distance from center and friction.
            Acceleration is determined by acceleration constant aC and friction constant fC where aC determines how fast an
            object's acceleration increases with distance from its target, and fC is the amount of friction, i.e the
            norm of the friction vector. An object's acceleration is the sum of a pull-vector directed towards the target
            and a friction-vector in opposite direction of the velocity.
         If collisions is True, the velocities of the new objects will be adjusted according to internal collisions. No collisions for squares.
 -}
stepObjects :: Float -> Objects -> Float -> Float -> Bool -> Objects
stepObjects t (Os s os) aC fC True = Os s $ collisionCorrections s t aC fC $ map (moveObject t aC fC) os
stepObjects t (Os s os) aC fC False = Os s $ map (moveObject t aC fC) os

--------------------------------------------------------------------move

--moveObject t aC fC o
--post: o with updated position and velocity based on t and some constants.
moveObject :: Float -> Float -> Float -> Object -> Object
moveObject t aC fC o@(O pic d (x,y) (vx,vy) (tx,ty)) =
                                                let
                                                   (px,py) = pull x y tx ty aC fC
                                                in
                                                   if distance x y tx ty < 1 && norm vx vy < 0.5 --there?
                                                      then O pic d (tx,ty) (0,0) (tx,ty)
                                                      else if (vx,vy) == (0,0)                       --object still?
                                                            then if norm px py <= fC               --object still after step?
                                                               then O pic d (x,y) (0,0) (tx,ty)
                                                               else  let
                                                                        (fx,fy) = fric px py fC
                                                                        (ax,ay) = (px+fx, py+fy)
                                                                        (vx',vy') = (vx + ax*t, vy + ay*t)
                                                                        (x',y') = newPos o t
                                                                     in
                                                                        O pic d (x',y') (vx',vy') (tx,ty)
                                                            else  let
                                                                     (fx,fy) = fric vx vy fC
                                                                  in
                                                                     if norm (vx + px*t) (vy + py*t) <= fC*t  --object still after step?
                                                                        then O pic d (x,y) (0,0) (tx,ty)
                                                                        else  let
                                                                                 (ax,ay) = (px+fx, py+fy)
                                                                                 (vx',vy') = (vx + ax*t, vy + ay*t)
                                                                                 (x',y') = newPos o t
                                                                              in
                                                                                 O pic d (x',y') (vx',vy') (tx,ty)

--newPos oldO time
--post: new object position based on old object oldO, time and old object's velocity
newPos :: Object -> Float -> (Float,Float)
newPos (O _ _ (x,y) (vx,vy) _) t = (x+vx*t, y+vy*t)


--pull x y tx ty aC fC
--post: (px,py) force given by distance from target t, controlled by aC, fC
pull :: Float -> Float -> Float -> Float -> Float -> Float -> (Float,Float)
pull x y tx ty aC fC = if (tx-x, ty-y) == (0,0) || aC == 0
                        then (0,0)
                        else  let
                                 (ex,ey) = normalize (tx-x) (ty-y)
                                 a = fC + aC * distance x y tx ty
                              in
                                 (a*ex, a*ey)

--fric vx vy fC
--post: frictional force based on fC and direction of v
fric :: Float -> Float -> Float -> (Float,Float)
fric vx vy fC = if (vx,vy) == (0,0)
                  then (0,0)
                  else  let
                           (evx,evy) = normalize vx vy
                        in
                           (-fC*evx, -fC*evy)

--distance px py qx qy
--post: distance bwtween p and q
distance :: Float -> Float -> Float -> Float -> Float
distance px py qx qy = norm (qx-px) (qy-py)

--normalize vx vy
--pre: (vx,vy) /= (0,0)
--post: v normalized
normalize :: Float -> Float -> (Float,Float)
normalize vx vy = (vx / norm vx vy, vy / norm vx vy)

norm :: Float -> Float -> Float
norm x y = sqrt (x^2 + y^2)

--------------------------------------------------------------------collisions

--collisionCorrections s t aC fC os
--post: os with every ball corrected based on shape s
collisionCorrections :: Shape -> Float -> Float -> Float -> [Object] -> [Object]
collisionCorrections _ _ _ _ [] = []
collisionCorrections _ _ _ _ [o] = [o]
collisionCorrections s t aC fC (o1:os) = case first (collide s o1) os of
                                          Nothing -> o1 : collisionCorrections s t aC fC os
                                          Just (o2,os') ->  let
                                                               (o1c,o2c) = collisionCorrection s t aC fC o1 o2
                                                            in
                                                               o1c : o2c : collisionCorrections s t aC fC os'

--collisionCorrection s t aC fC o1 o2
--pre: collision happens between o1 and o2
--post: (o1,o2) corrected based on shape s, with speed bosts for slow collisions determined by t, aC and fC
collisionCorrection :: Shape -> Float -> Float -> Float -> Object -> Object -> (Object,Object)
collisionCorrection Ball t aC fC o1@(O p1 d (x1,y1) (vx1,vy1) t1) o2@(O p2 _ (x2,y2) (vx2,vy2) t2) =
                        let
                           (vx1',vy1') = speedBoost o1 t aC fC
                           (vx2',vy2') = speedBoost o2 t aC fC
                           
                           (unx, uny) = normalize (x2-x1) (y2-y1)
                           (utx, uty) = (-uny, unx)
                           v1n = unx*vx1' + uny*vy1'
                           v2n = unx*vx2' + uny*vy2'
                           v1n' = v2n
                           v2n' = v1n
                           v1t' = utx*vx1' + uty*vy1'
                           v2t' = utx*vx2' + uty*vy2'
                           
                           (v1nx',v1ny') = (v1n'*unx, v1n'*uny)
                           (v1tx',v1ty') = (v1t'*utx, v1t'*uty)
                           (v2nx',v2ny') = (v2n'*unx, v2n'*uny)
                           (v2tx',v2ty') = (v2t'*utx, v2t'*uty)
                           
                           (vx1'',vy1'') = (v1nx'+v1tx', v1ny'+v1ty')
                           (vx2'',vy2'') = (v2nx'+v2tx', v2ny'+v2ty')
                        in
                           (O p1 d (x1,y1) (vx1'',vy1'') t1, O p2 d (x2,y2) (vx2'',vy2'') t2)
                           
collisionCorrection Square t aC fC o1 o2 = (o1,o2)

--speedBoost o t aC fC
--post: speed bosted if speed not higher than pull
speedBoost :: Object -> Float -> Float -> Float -> (Float,Float)
speedBoost (O _ _ (x,y) (vx,vy) (tx,ty)) t aC fC = if norm vx vy <= t * 2 * uncurry norm (pull x y tx ty aC fC)
                                                         then (vx*10, vy*10)
                                                         else (vx,vy)


--collide b1 b2
--post: true iif collision
collide :: Shape -> Object -> Object -> Bool
collide Ball o1@(O _ dim (x1,y1) (vx1,vy1) _) o2@(O _ _ (x2,y2) (vx2,vy2) _) =
                        let
                           (x1',y1') = newPos o1 0.0001
                           (x2',y2') = newPos o2 0.0001
                           dist = distance x1 y1 x2 y2
                           dist' = distance x1' y1' x2' y2'
                        in
                           dist < dim && dist > dist'
                           
collide Square o1 o2 = False


--first f xs
--post: Just ('first element in xs making f true', 'rest of xs'), or Nothing
first :: (a -> Bool) -> [a] -> Maybe (a, [a])
first f xs = aux f xs []
   where
      aux _ [] _ = Nothing
      aux f (x:xs) acc = if f x
                           then Just (x, acc ++ xs)
                           else aux f xs (x:acc)

--TEST CASES-----------------------------------------------------------------------------------------------------------------------------------------------------

testgetObjectData = TestCase (assertEqual "getObjectData 1.1 2.2 3 4 5 6 7 0.8" (2.5666666,-4.125,2.0533333) (getObjectData 1.1 2.2 3 4 5 6 7 0.8))

test1Pull = TestCase (assertEqual "pull 46.6 34.2 46.6 34.2 569 591" (0,0) (pull 46.6 34.2 46.6 34.2 569 591))
test2Pull = TestCase (assertEqual "pull 5 10 5 4 0 100" (0,0) (pull 5 10 5 4 0 100))
test3Pull = TestCase (assertEqual "pull 5 10 (-5) 4 2 100" (-105.7493,-63.44958) (pull 5 10 (-5) 4 2 100))

testFric = TestCase (assertEqual "fric 0 0 56" (0,0) (fric 0 0 56))

testgetAnimationDim = TestCase (assertEqual "getAnimationDim 2 2 3 3 0.1" (0.2,0.2) (getAnimationDim 2 2 3 3 0.1))

testMoveObject = TestCase (assertEqual "moveObject 1 2 200 (O (circleSolid 5) 10 (0,100) (0,10) (50,50))" (O (ThickCircle 2.5 5.0) 10.0 (0.0,110.0) (241.42136,-431.42136) (50.0,50.0)) (moveObject 1 2 200 (O (circleSolid 5) 10 (0,100) (0,10) (50,50)))) 
test2MoveObject = TestCase (assertEqual "moveObject 1 2 200 (O (circleSolid 5) 10 (50,50) (0,10) (50,50)" (O (ThickCircle 2.5 5.0) 10.0 (50.0,50.0) (0.0,0.0) (50.0,50.0)) (moveObject 1 2 200 (O (circleSolid 5) 10 (50,50) (0,10) (50,50))))

testCollisionCorrection = TestCase (assertEqual "testCollisionCorrection" ((O (ThickCircle 2.5 5.0) 10.0 (0.0,100.0) (-0.0,-100.0) (50.0,50.0),O (ThickCircle 2.5 5.0) 10.0 (0.0,109.0) (0.0,100.0) (50.0,50.0))) (collisionCorrection Ball 1 2 200 (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,-10) (50,50))))

test1SpeedBoost = TestCase (assertEqual "speedBoost (O (circleSolid 5) 10 (0,100) (0, 1) (50,50)) 1 0.5 200" (0, 10) (speedBoost (O (circleSolid 5) 10 (0,100) (0, 1) (50,50)) 1 0.5 200))
test2SpeedBoost = TestCase (assertEqual "speedBoost (O (circleSolid 5) 10 (0,100) (0, 1000) (50,50)) 1 0.5 200" (0, 1000) (speedBoost (O (circleSolid 5) 10 (0,100) (0, 1000) (50,50)) 1 0.5 200))

test1Collide = TestCase (assertEqual "collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,110) (0,-10) (50,50))" False (collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,110) (0,-10) (50,50))))
test2Collide = TestCase (assertBool "collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,-10) (50,50))" (collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,-10) (50,50))))
test3Collide = TestCase (assertEqual "collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,10) (50,50))" False (collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,10) (50,50))))
test4Collide = TestCase (assertBool "collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,9) (50,50))" (collide Ball (O (circleSolid 5) 10 (0,100) (0,10) (50,50)) (O (circleSolid 5) 10 (0,109) (0,9) (50,50))))

test1First = TestCase (assertEqual "first (\a -> a < 2) [2,3,4,1]" (Just (1,[4, 3, 2])) (first (\a -> a < 2) [2,3,4,1]))
test2First = TestCase (assertEqual "first (\a -> a < 2) [2,3,4,1]" (Just (1,[4, 3, 2])) (first (\a -> a < 2) [2,3,4,1]))

--tests to export
objectTest = TestList [testgetObjectData, test1Pull, test2Pull, test3Pull, testFric, testgetAnimationDim, testMoveObject, test2MoveObject, testCollisionCorrection, test1SpeedBoost, test2SpeedBoost, test1Collide, test2Collide, test3Collide, test4Collide, test1First, test2First]