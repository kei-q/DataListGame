module Physics where

import qualified Physics.Hipmunk as H

-- | Constructs a Vector.
(+:) :: H.CpFloat -> H.CpFloat -> H.Vector
(+:) = H.Vector
infix 4 +:

type Space = H.Space
type Pos = (Double,Double)
type Angle = Double
data Shape = Wall Pos Pos
           | Trash Int Pos Angle
type Obj = IO Shape

initPhysics = do
  H.initChipmunk
  space <- H.newSpace
  H.setElasticIterations space 10
  H.setGravity space (0 +: 230)
  wl <- buildWall space (0,0)   (0,480)
  wb <- buildWall space (0,480) (640,480)
  wr <- buildWall space (640,0) (640,480)
  let step = H.step space 9.99e-3 -- 3.33e-3
  return (space,step)

buildWall :: H.Space -> Pos -> Pos -> IO Obj
buildWall space p1@(x1,y1) p2@(x2,y2) = do
  static <- H.newBody H.infinity H.infinity
  H.setPosition static (0 +: 0)
  let seg = H.LineSegment (x1 +: y1) (x2 +: y2) 1
  shape <- H.newShape static seg 0
  H.setFriction shape 1.0
  H.setElasticity shape 0.6
  H.spaceAdd space (H.Static shape)
  let get = return $ Wall p1 p2
  return get

buildTrash :: H.Space
              -> Int
              -> H.AngVel
              -> H.Distance
              -> Pos
              -> IO Obj
buildTrash space c angVel radius pos@(x1,y1) = do
  let mass = 20 
      t    = H.Circle radius
  b <- H.newBody mass $ H.momentForCircle mass (0, radius) 0
  s <- H.newShape b t 0
  H.setAngVel b angVel
  H.setPosition b (x1+:y1)
  H.setFriction s 0.5
  H.setElasticity s 0.9

  H.spaceAdd space b
  H.spaceAdd space s
  let get = do
        H.Vector px py <- H.getPosition $ H.getBody s
        angle          <- H.getAngle    $ H.getBody s
        return $ Trash c (px,py) angle
  return get

