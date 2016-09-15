import qualified Geometry.Cube as Cube
import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid

sphereVolume :: Float -> Float
sphereVolume r = Sphere.volume r

sphereArea :: Float -> Float
sphereArea r = Sphere.area r

cubeVolume :: Float -> Float
cubeVolume a = Cube.volume a

cubeArea :: Float -> Float
cubeArea a = Cube.area a

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = Cuboid.volume a b c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = Cuboid.area a b c
