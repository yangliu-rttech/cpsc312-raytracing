module Main where

main :: IO ()
main = print "test"

type Vec = (Double,Double,Double)

vmap :: (Double -> Double) -> Vec -> Vec
vmap f (x,y,z) = (f x, f y, f z)

--vzip :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
vzip :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
vzip f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

(+.) :: Vec -> Vec -> Vec
(+.) = vzip (+)

(-.) :: Vec -> Vec -> Vec
(-.) = vzip (-)

(*.) :: Vec -> Double -> Vec 
(*.) v d = vmap (*d) v

(/.) :: Vec -> Double -> Vec
(/.) v d = vmap (/d) v

epsilon :: Double
epsilon = 1e-12

--approximately equals (within epsilon)
(~=) :: Double -> Double -> Bool 
(~=) x y = abs(x-y) < epsilon

--vector approximately equals
(=.) :: Vec -> Vec -> Bool 
(=.) p1 p2 = case vzip (~=) p1 p2 of
   (True,True,True) -> True 
   _ -> False

-- >>> (1,1,1) =. (0.999999999999999,0.999999999999999,1.00000000000001)
-- True

--dot product
dot :: Vec -> Vec -> Double
dot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

--cross product
cross :: Vec -> Vec -> Vec
cross (x1,y1,z1) (x2,y2,z2) = (y1*z2 - z1*y2, z1*x2-x1*z2,x1*y2-y1*x2)

quadrance :: Vec -> Double 
quadrance v = v `dot` v

mag :: Vec -> Double
mag v = sqrt (quadrance v)

normalize :: Vec -> Vec 
normalize v = v /. mag v

--the Vecs are the rows
type Mat3 = (Vec,Vec,Vec) 

--matrix multiplying vector
(*..) :: Mat3 -> Vec -> Vec
(*..) ((a11,a12,a13),
       (a21,a22,a23),
       (a31,a32,a33)) (x,y,z) = 
    (x*a11 + y*a12 + z*a13,
     x*a21 + y*a22 + z*a23,
     x*a31 + y*a32 + z*a33)

transp :: Mat3 -> Mat3 
transp ((a11,a12,a13),
        (a21,a22,a23),
        (a31,a32,a33)) = 
       ((a11,a21,a31),
        (a12,a22,a32),
        (a13,a23,a33))

--matrix multiplied by matrix
(*...) :: Mat3 -> Mat3 -> Mat3 
(*...) m1 m2 = let (c1,c2,c3) = transp m2 in 
   transp (m1 *.. c1, m1 *.. c2, m1 *.. c3)
--because transpose lets you get and set the columns instead of the rows

rotxy :: Double -> Mat3 
rotxy th = ((cos th, -sin th, 0),
            (sin th,  cos th, 0),
            (0,       0,      1))

rotxz :: Double -> Mat3
rotxz th = ((cos th, 0, -sin th),
            (0,      1,       0),
            (sin th, 0,  cos th))

rotyz :: Double -> Mat3
rotyz th = ((1, 0,       0),
            (0, cos th, -sin th),
            (0, sin th,  cos th))

-- 3 in 1
rot_xy_xz_yz :: Double -> Double -> Double -> Mat3
rot_xy_xz_yz xy xz yz = rotxy xy *... rotxz xz *... rotyz yz

--point and normal
data Plane = Plane {pnt, nrm :: Vec} 

threePointsToPlane :: Vec -> Vec -> Vec -> Plane 
threePointsToPlane a b c = Plane {pnt = (a +. b +. c) /. 3, nrm = norm}
   where norm = normalize ((b -. a) `cross` (c -. a))

data Ray = Ray {pos, look :: Vec}

parallelTo :: Ray -> Plane -> Bool 
parallelTo ry pl = nrm pl `dot` look ry ~= 0

--https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-plane-and-ray-disk-intersection
--returns maybe the r such that the point pos + r*look is on the plane
planeIntersectDist :: Plane -> Ray -> Maybe Double 
planeIntersectDist pl ry = if ry `parallelTo` pl then Nothing 
   else Just (((pnt pl -. pos ry) `dot` nrm pl) / (look ry `dot` nrm pl))

--reflects the direction of a given vector off a given plane
reflectDirOff :: Plane -> Vec -> Vec 
reflectDirOff pl dir = 
   let componentOfDirInDirectionOfNormal = nrm pl *. (nrm pl `dot` dir) in  
   dir -. (componentOfDirInDirectionOfNormal *. 2)

data Sphere = Sphere {radius :: Double, center :: Vec}

sphereIntersectDist :: Sphere -> Ray -> Maybe Double
sphereIntersectDist sph ry = 
   let r2 = radius sph * radius sph in 
   let center2pos = center sph -. pos ry in 
   let distanceFromCenterToPosSquared = quadrance center2pos in 
   let loc = center2pos `dot` look ry in 
   let loc2 = loc * loc in 
   let back2frontDistSquared = loc2 - distanceFromCenterToPosSquared + r2 in 
   if back2frontDistSquared < 0 then Nothing else
      let back2frontDist = sqrt back2frontDistSquared in 
      let ans1 = -loc - back2frontDist in 
      --ans1 is the distance to the front of the sphere and ans2 the distance to the back of the sphere
      --if you're not inside the sphere, you want ans1
      if ans1 > 0 then Just ans1 else 
         let ans2 = -loc + back2frontDist in 
         Just ans2

data RayResult = NoHit |
   Hit {dist :: Float, color :: Vec}

type Traceable = Ray -> RayResult

data Camera = Camera {position :: Vec, fov, yaw, pitch :: Float}