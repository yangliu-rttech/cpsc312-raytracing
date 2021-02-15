main :: IO ()
main = print "test"

type Vec = (Double,Double,Double)

vmap :: (Double -> Double) -> Vec -> Vec
vmap f (x,y,z) = (f x, f y, f z)

vzip :: (Double -> Double -> Double) -> Vec -> Vec -> Vec
vzip f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)

(+.) :: Vec -> Vec -> Vec
(+.) = vzip (+)

(-.) :: Vec -> Vec -> Vec
(-.) = vzip (-)

(*.) :: Vec -> Double -> Vec 
(*.) v d = vmap (*d) v

(/.) :: Vec -> Double -> Vec
(/.) v d = vmap (/d) v

--dot product
dot :: Vec -> Vec -> Double
dot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

--cross product
cross :: Vec -> Vec -> Vec
cross (x1,y1,z1) (x2,y2,z2) = (y1*z2 - z1*y2, z1*x2-x1*z2,x1*y2-y1*x2)

mag :: Vec -> Double
mag v = sqrt (dot v v)

--the Vecs are the rows
type Mat3 = (Vec,Vec,Vec) 

(*..) :: Mat3 -> Vec -> Vec
(*..) ((a11,a12,a13),
       (a21,a22,a23),
       (a31,a32,a33)) (x,y,z) = 
    (x*a11 + y*a12 + z*a13,
     x*a21 + y*a22 + z*a23,
     x*a31 + y*a32 + z*a33)

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