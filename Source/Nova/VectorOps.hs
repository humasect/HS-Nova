{-# OPTIONS_GHC -XTypeSynonymInstances #-}
module Nova.VectorOps where

import Nova.GL

type Scalar = GLdouble -- or GLfloat

type V2     = (Scalar,Scalar)
type V3     = (Scalar,Scalar,Scalar)
type V4     = (Scalar,Scalar,Scalar,Scalar)

vertex2v    (x,y)     = vertex2 x y
vertex3v    (x,y,z)   = vertex3 x y z
color4v     (r,g,b,a) = color4 r g b a
texCoord2v  (u,v)     = texCoord2 u v

translate2v (x,y)     = translate x y 0
scalev      (x,y,z)   = scale x y z
rotatev a   (x,y,z)   = rotate a x y z

{-
-- GLfloat
vertex2     = vertex2f
vertex3     = vertex3f
color3      = color3f
color4      = color4f
texCoord2   = texCoord2f
translate   = translatef
rotate      = rotatef
scale       = scalef
-}

-- GLdouble
vertex2     = vertex2d
vertex3     = vertex3d
color3      = color3d
color4      = color4d
texCoord2   = texCoord2d
translate   = translated
rotate      = rotated
scale       = scaled

class {-VectorOps a => -} TupleModding a where
    mod1 :: (Scalar -> Scalar) -> a -> a

instance TupleModding V2 where
    mod1 = mod1Tuple2

mod1Tuple2 :: Num a => (a -> b) -> (a,a) -> (b,b)
mod1Tuple2 f (a,b) = (f a,f b)

mod2Tuple2 :: (a -> b -> c) -> (a,a) -> (b,b) -> (c,c)
mod2Tuple2 f (a,b) (c,d) = (a `f` c,b `f` d)

mod1Tuple3 :: (a -> b) -> (a,a,a) -> (b,b,b)
mod1Tuple3 f (a,b,c) = (f a,f b,f c)

mod2Tuple3 :: (a -> b -> c) -> (a,a,a) -> (b,b,b) -> (c,c,c)
mod2Tuple3 f (a,b,c) (z,y,x) = (a `f` z,b `f` y,c `f` x)

mod3Tuple3 :: (a -> b -> c -> d) -> (a,a,a) -> (b,b,b) -> (c,c,c) -> (d,d,d)
mod3Tuple3 f (a,b,c) (z,y,x) (q,w,e) = (f a z q,f b y w,f c x e)

class (Fractional a,Num a) => VectorOps a where
    vecOf           :: Scalar -> a
    dot             :: a -> a -> Scalar
    rot             :: a -> Scalar -> a

    normalOf        :: a -> a
    magnitudeOf     :: a -> Scalar
    rotAround       :: a -> Scalar -> a -> a
    average         :: [a] -> a

    linearLerp      :: a -> a -> Scalar -> a
    quadraticLerp   :: a -> a -> a -> Scalar -> a
    cubicLerp       :: a -> a -> a -> a -> Scalar -> a

    snapToGrid      :: a -> Scalar -> a

    rotAround a t b = (((a - b) `rot` t) + b)
    normalOf v      = let m = magnitudeOf v in if m>0 then v / vecOf m else v
    magnitudeOf v   = sqrt (v `dot` v)
  --average []      = vecOf 0
    average vs      = (foldl1 (+) vs) / vecOf (fromIntegral $ length vs)

instance VectorOps Scalar where
    vecOf s                  = s
    a `dot` b                = undefined
    a `rot` angle            = undefined

    a `snapToGrid` sz        = (fromIntegral $ round (a / sz)) * sz

    linearLerp a b t         = a+(b-a)*t
    quadraticLerp p0 p1 p2 t = ((1-t)^2)*p0 + 2*t*(1-t)*p1 + (t^2)*p2
    cubicLerp p0 p1 p2 p3 t  = ((1-t)^3)*p0 + 3*t*(1-t)^2*p1 + 3*t^2*(1-t)*p2 + t^3*p3

{-
data Vec2 a = Vec2 a a
instance Functor Vec2 where
    fmap f (Vec2 x y) = Vec2 (f x) (f y)
-}

instance Num V2 where
    (ax,ay) + (bx,by) = (ax+bx        ,ay+by        )
    (ax,ay) - (bx,by) = (ax-bx        ,ay-by        )
    (ax,ay) * (bx,by) = (ax*bx        ,ay*by        )
    abs         (x,y) = (abs x        ,abs y        )
    signum      (x,y) = (signum x     ,signum y     )
    fromInteger i     = (fromInteger i,fromInteger i)
    negate      (x,y) = (negate x     ,negate y     )

instance Fractional V2 where
    (ax,ay) / (bx,by) = (ax/bx         ,ay/by         )
    fromRational r    = (fromRational r,fromRational r)

toRadians :: Scalar -> Scalar
toRadians d = d * (pi/180)


instance VectorOps V2 where
    (a0,a1) `dot`        (b0,b1) = a0*b0 + a1*b1
    (x,y)   `rot`        angle   = let t = toRadians angle in
                                   ((cos t)*x - (sin t)*y, (sin t)*x + (cos t)*y)
    vecOf                s       = (s                    ,s                     )
    (x,y)   `snapToGrid` sz      = (x `snapToGrid` sz    , y `snapToGrid` sz    )

    linearLerp    (ax,ay) (bx,by)                         t = (linearLerp ax bx t         , linearLerp ay by t)
    quadraticLerp (p0x,p0y) (p1x,p1y) (p2x,p2y)           t = (quadraticLerp p0x p1x p2x t, quadraticLerp p0y p1y p2y t)
    cubicLerp     (p0x,p0y) (p1x,p1y) (p2x,p2y) (p3x,p3y) t = (cubicLerp p0x p1x p2x p3x t, cubicLerp p0y p1y p2y p3y t)

v2cross :: V2 -> V2 -> Scalar
v2cross (a0,a1) (b0,b1) = a0*b1 - b0*a1

instance Num V4 where
    fromInteger                 i = let r = fromInteger i in
                                    (r       ,r       ,r       ,r    )
    (a0,a1,a2,a3) + (b0,b1,b2,b3) = (a0+b0   ,a1+b1   ,a2+b2   ,a3+b3)
    (a0,a1,a2,a3) * (b0,b1,b2,b3) = (a0*b0   ,a1*b1   ,a2*b2   ,a3*b3)
    (a0,a1,a2,a3) - (b0,b1,b2,b3) = (a0-b0   ,a1-b1   ,a2-b2   ,a3-b3)
    abs                 (r,g,b,a) = (abs    r,abs    g,abs    b,abs    a)
    signum              (r,g,b,a) = (signum r,signum g,signum b,signum a)
    negate              (r,g,b,a) = (negate r,negate g,negate b,negate a)

instance Fractional V4 where
    (a0,a1,a2,a3) / (b0,b1,b2,b3) = (a0/b0,a1/b1,a2/b2,a3/b3)
    fromRational                r = let r' = fromRational r in
                                    (r'   ,r'   ,r'   ,r'   )

instance VectorOps V4 where
    vecOf s    = (s,s,s,s)
    dot        = undefined
    rot        = undefined
    snapToGrid = undefined

    linearLerp (ax,ay,aw,aq) (bx,by,bw,bq) t =
        (linearLerp ax bx t         ,linearLerp ay by t         ,
         linearLerp aw bw t         , linearLerp aq bq t        )
    quadraticLerp (p0x,p0y,p0z,p0w) (p1x,p1y,p1z,p1w) (p2x,p2y,p2z,p2w) t =
        (quadraticLerp p0x p1x p2x t,quadraticLerp p0y p1y p2y t,
         quadraticLerp p0z p1z p2z t,quadraticLerp p0w p1w p2w t)
    cubicLerp (p0x,p0y,p0z,p0w) (p1x,p1y,p1z,p1w) (p2x,p2y,p2z,p2w) (p3x,p3y,p3z,p3w) t =
        (cubicLerp p0x p1x p2x p3x t,cubicLerp p0y p1y p2y p3y t,
         cubicLerp p0z p1z p2z p3z t,cubicLerp p0w p1w p2w p3w t)

----------------

{-
instance VectorOps V3 where
    (a0,a1,a2) <-> (b0,b1,b2) = (a0-b0, a1-b1, a2-b2)
    (a0,a1,a2) <+> (b0,b1,b2) = (a0+b0, a1+b1, a2+b2)
    (x,y,z) <*> f = (x*f, y*f, z*f)
    (x,y,z) </> f = (x/f, y/f, z/f)
    (a0,a1,a2) <.> (b0,b1,b2) = a0*b0 + a1*b1 + a2*b2
    (a0,a1,a2) <=> (b0,b1,b2) = a0==b0 && a1==b1 && a2==b2
    rotated = undefined

v3cross :: V3 -> V3 -> V3
v3cross (a0,a1,a2) (b0,b1,b2) = (x,y,z) where
    x = a1 * b2 - a2 * b1
    y = a2 * b0 - a0 * b2
    z = a0 * b1 - a1 * b0
-}

