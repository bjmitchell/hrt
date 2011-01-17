module Geometry where

data Vector3 = Vector3 Float Float Float deriving Show

getX::Vector3 -> Float
getX (Vector3 x _ _ ) = x

getY::Vector3 -> Float
getY (Vector3 _ y _ ) = y

getZ::Vector3 -> Float
getZ (Vector3 _ _ z ) = z

dot::Vector3 -> Vector3 -> Float
dot a b = x + y + z where
                     x = getX a * getX b
                     y = getY a * getY b
                     z = getZ a * getZ b

(<+>)::Vector3 -> Vector3 -> Vector3
(<+>) a b = Vector3 x y z where
                     x = getX a + getX b
                     y = getY a + getY b
                     z = getZ a + getZ b

mult::Float -> Vector3 -> Vector3
mult a b = Vector3 x y z where
                     x = a * getX b
                     y = a * getY b
                     z = a * getZ b

mag::Vector3 -> Float
mag a = sqrt ( a `dot` a )

unit::Vector3 -> Vector3
unit a = (1.0/mag a) `mult` a

data Plane = Plane Vector3 Vector3

getPoint::Plane -> Vector3
getPoint (Plane p n ) = p

getNormal::Plane -> Vector3
getNormal (Plane p n ) = n


