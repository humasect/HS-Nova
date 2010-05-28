module ShmupTest.Ship where

import Nova.HumaMath
import qualified Nova.Draw as Draw
import Nova.GL
import Nova.Game (Input)
import qualified Nova.Game as Input

import ShmupTest.Particles

data Ship
	= Player {
		hp :: Int,
		pos :: V2,
		bounds :: V2,
		actDelay :: Scalar,
		invTime :: Scalar }
	| AutoDrone {
		hp :: Int, -- negative when drawn red. (flashing) FIXME
		pos :: V2,
		bounds :: V2,
		actDelay :: Scalar,
		nextAct :: CompassDirection }
	| Drone {
		hp :: Int,
		pos :: V2,
		bounds :: V2,
		actDelay :: Scalar }
{-	| TestBoss {
		hp :: Int,
		pos :: V2,
		bounds :: V2,
		actDelay :: GLfloat,
		nextAct :: TestBossAct }  -}
	deriving (Show,Read)

--data TestBossAct = SwirlBullets | 

{-
class ShipAI a where
	new :: V2 -> a
	move :: Input -> GLfloat -> a -> a
	act :: Input -> GLfloat -> a -> a
	hit :: Bullet -> a -> Bool

instance ShipAI Player where
	new v = Player { bounds=(4,4), pos=v, actDelay = 0 }

instance ShipAI Drone where
	new v = Drone { hp=10, bounds=(16,16), pos=v, actDelay=0, nextShot=South }
-}

expTime :: Ship -> Scalar
expTime _ = 400/1000
{-
expTime (Player {}) = 100/1000
expTime (Drone {}) = 100/1000
expTime (AutoDrone {}) = 100/1000
-}

shipBeingShot :: [Bullet] -> Ship -> Bool
shipBeingShot bs s = any (bulletTouchingShip s) bs

bulletTouchingShip :: Ship -> Bullet -> Bool
bulletTouchingShip ship b =
	let l = loc b - pos ship in isCircleInBox (l,radius b) (bounds ship)

hitBullet :: [Ship] -> Bullet -> Bool
hitBullet ss b = any (\s-> bulletTouchingShip s b) ss

newPlayer v it = Player { hp=1, bounds=(2,2), pos = v, actDelay = 0, invTime = it }

newAutoDrone v = AutoDrone { hp=30, bounds=(16,16), pos=v, actDelay=0, nextAct=South }

newDrone v = Drone { hp=15, bounds=(16,16), pos=v, actDelay=0 }

moveShip :: Input -> Scalar -> Ship -> Ship
moveShip input tick ship@(Player {pos=p, actDelay=ad, invTime=it}) =
	let
		speed = (if (input `Input.charState` 'z') then 200 else 400) * tick
		nextAd = if (not$input `Input.charState` 'z') then 0 else if (ad <= 0) then (100/1000) else (ad-tick)
	in ship {
		actDelay = nextAd,
		invTime = it - tick,
		pos = keepPointInBox (300-16, 400-16) (p + (Input.moveVector input * vecOf speed))
	}

moveShip player tick ship@(AutoDrone {actDelay=ad, nextAct=ns}) =
	ship {
		actDelay = if ad <=0 then (300/1000) else (ad-tick),
		nextAct = if ad <=0 then succCompDir ns else ns
	}
moveShip player tick ship@(Drone {actDelay=ad}) =
	ship {
		actDelay = if ad <=0 then (300/1000) else (ad-tick)
	}

actShip :: Ship -> Input -> Scalar -> Ship -> [Bullet]
actShip player input tick ship@(Player {pos=(x,y), bounds=(bx,by), actDelay=ad}) =
	if input `Input.charState` 'z' && (ad <= 0) then [
		Shot {loc=(x-12,y+16), speed=1200, radius=5},
		Shot {loc=(x+12,y+16), speed=1200, radius=5}
	] else []

actShip player input tick ship@(AutoDrone {pos=(x,y), actDelay=ad, nextAct=cd}) =
	let d = (vectorForCompDir cd) * vecOf 300 in
	if ad<=0 then [Bullet {loc=(x,y), dir=d, radius=5}] else []

actShip player input tick ship@(Drone {pos=(x,y), actDelay=ad}) = let
	angle = angleForPoints (x,y) (pos player) 
	d = (cos angle, sin angle) * vecOf 300
	in
	if ad<=0 then [Bullet {loc=(x,y), dir=d, radius=5}] else []

drawShip :: Ship -> IO ()
drawShip ship@(Player {pos=(x,y)}) = do
	pushMatrix
	translate x y 0
	let a = if invTime ship > 0 then 0.2 else 1

	color4 0.33 0.33 0.33 a
	Draw.boxAt Quads (11,11) (0,0)

	color4 0.8 0.8 0.8 a
	Draw.primitiveDo Quads $ do
		mapM_ vertex2v shipSide
		mapM_ vertex2v [(-x,y) | (x,y) <- reverse shipSide]

	color4 1 1 1 a
	begin & Triangles
	vertex2 8 16  >> vertex2 11 (-23) >> vertex2 8 (-28)
	vertex2 (-8) 16 >> vertex2 (-8) (-28) >> vertex2 (-11) (-23)
	end

	-- collide box
	color4 1 0 0 a
	Draw.boxAt Quads (bounds ship) (0,0)
	popMatrix
	where
	shipSide = [
		(4, (-24)),
		(24, (-16)),
		(16, 16),
		(4, 24)
		]

drawShip ship@(Drone {pos=(x,y)}) = do
	pushMatrix >> translate x y 0
	if (hp ship < 0) then color3 1 1 1 else color3 0.33 0.33 0.33
	let (w,h) = bounds ship
	begin & Triangles
	vertex2 (-(w/2)) h
	vertex2 0 (-h)
	vertex2 (w/2) h
	end >> popMatrix

drawShip ship@(AutoDrone {pos=(x,y)}) = do
	pushMatrix >> translate x y 0
	if (hp ship < 0) then color3 1 1 1 else color3 0.33 0.33 0.33
	Draw.boxAt Quads (bounds ship) (0,0)
	rotate 45 0 0 1
	Draw.boxAt Quads (bounds ship) (0,0)
	popMatrix

defaultHitShip :: [Bullet] -> Ship -> Maybe Ship
defaultHitShip bs ship = 
	let h = (hp ship) - (length $ filter (bulletTouchingShip ship) bs) in
	if h <= 0 then Nothing else Just ship { hp=h }

hitShip :: [Bullet] -> Ship -> Maybe Ship

hitShip bs ship@(Player {}) =
	if invTime ship <= 0 && any (bulletTouchingShip ship) bs then Nothing else Just ship

hitShip bs ship@(Drone {}) = defaultHitShip bs ship
hitShip bs ship@(AutoDrone {}) = defaultHitShip bs ship
