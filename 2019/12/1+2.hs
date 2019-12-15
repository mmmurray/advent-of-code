import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set

type Vec3 = (Int, Int, Int)
type Body = (Vec3, Vec3)

compareVec3 :: Vec3 -> Vec3 -> Vec3
compareVec3 (x1,y1,z1) (x2,y2,z2) = (c x1 x2, c y1 y2, c z1 z2)
  where
    c a b = case compare a b of
      LT -> 1
      EQ -> 0
      GT -> -1

addVec3 :: Vec3 -> Vec3 -> Vec3
addVec3 (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)

vecMag :: Vec3 -> Int
vecMag (x,y,z) = (abs x) + (abs y) + (abs z)

getVelocityChange :: [Body] -> Vec3 -> Vec3
getVelocityChange bodies p1 = List.foldl (\acc (p2,_) -> addVec3 acc (compareVec3 p1 p2)) (0,0,0) bodies

applyGravity :: [Body] -> [Body]
applyGravity bodies = map (\(p,v) -> (p, addVec3 v $ getVelocityChange bodies p)) bodies

applyVelocity :: [Body] -> [Body]
applyVelocity bodies = map (\(p,v) -> (addVec3 p v, v)) bodies

tick :: [Body] -> [Body]
tick bodies = applyVelocity $ applyGravity bodies

tickTimes :: Int -> [Body] -> [Body]
tickTimes 0 bodies = bodies
tickTimes n bodies = tick $ tickTimes (n - 1) bodies

ticksUntilRepeat :: [Body] -> Int
ticksUntilRepeat bodies = f bodies 0 Set.empty
  where f bodies n seen =
          let
            newBodies = tick bodies
          in if Set.member newBodies seen then n else f newBodies (n + 1) (Set.insert newBodies seen)

body :: Vec3 -> Body
body p = (p, (0,0,0))

energy :: [Body] -> Int
energy bodies = sum $ map f bodies
  where
    f (p,v) = (vecMag p) * (vecMag v)

part2 :: [Body] -> Int
part2 bodies =
  let
    bodiesX = map (\((px,_,_),(vx,_,_)) -> ((px,0,0),(vx,0,0))) bodies
    bodiesY = map (\((_,py,_),(_,vy,_)) -> ((0,py,0),(0,vy,0))) bodies
    bodiesZ = map (\((_,_,pz),(_,_,vz)) -> ((0,0,pz),(0,0,vz))) bodies
    ttrX = ticksUntilRepeat bodiesX
    ttrY = ticksUntilRepeat bodiesY
    ttrZ = ticksUntilRepeat bodiesZ
  in
    lcm ttrX $ lcm ttrY ttrZ

main = do
  let initialBodies = map body [(4,12,13), (-9,14,-3), (-7,-1,2),(-11,17,-1)]
  print $ "Part 1: " ++ (show $ energy $ tickTimes 1000 initialBodies)
  print $ "Part 2: " ++ (show $ part2 initialBodies)
