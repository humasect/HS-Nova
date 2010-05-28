module ShmupTest.ShmupTest where

import Data.Maybe (catMaybes)
import Data.List (partition)

import Common
import Nova.Game hiding (Input(..))
import Nova.HumaMath
import qualified Nova.Draw as Draw
import Nova.GL
import Nova.Resource

import ShmupTest.Ship
import ShmupTest.Particles
import ShmupTest.Stage

type GameObject = Either Explosion Ship

data ShmupTest = ShmupTest {
	player :: GameObject,
	shots :: [Either Explosion Bullet],

	ships :: [GameObject],
	bullets :: [Bullet],
	bulletFlash :: Scalar,

	score :: Int,
	lives :: Int,
	continuesUsed :: Int,

	stage :: Stage,
	stageTime :: Scalar
	}
	deriving (Show,Read)

instance Game ShmupTest where
	listen = gameListen
	update = gameUpdate
	render = gameRender

	new = ShmupTest {
		player = Right (newPlayer (0,0) 0), shots=[],
		ships = [],
		score = 0, lives = 3, continuesUsed = 0,
		stage = newStage1, stageTime = 0,

		bullets=[],
		bulletFlash=0
		}

gameUpdate g input tick =
	--let (explosions,ships) =  partition ..
	g { player=moveObject (bullets g) revivePlayer,
		shots=newShots ++ (map moveShot cleanupShots),  --moveShots,
		ships=map (moveObject allShots) cleanupShips,
		bullets=newBullets ++ moveBullets,
		bulletFlash=checkBulletFlash,
		score=addScore,
		stageTime=(stageTime g) + (10*tick)
		}
	where
	checkBulletFlash = (if bulletFlash g > 0.75 then 0 else bulletFlash g) + (4 * tick)
	addScore =
		let count es = floor ((sum $ map (\e-> 1 - (explosionPerc e)) es))
		in (score g) + ((count allSparks)) + ((count allExplosions) * 10)
	nextWave = map (\s-> Right s) $ shipsForStageTime (stage g) (stageTime g)

	revivePlayer = case player g of
		Left l -> if (explosionPerc l) >= 1 then Right $ newPlayer (0,0) 2 else Left l
		Right r -> Right r

	cleanupShips =
		let shipsLeft = filter (\o -> case o of
			Left l -> if (explosionPerc l) >= 1 then False else True
			Right r -> True) $ ships g in
		if null shipsLeft then nextWave else shipsLeft

	cleanupShots = filter (\o -> case o of
		Left l -> if (explosionPerc l) >= 1 then False else True
		Right r -> if (loc r) `isPointInBox` (300,400) then True else False) $ shots g

	moveObject :: [Bullet] -> GameObject -> GameObject
{-
	moveObject bs o
		| Left (moveExplosion tick l) <- Left l
		| 
-}
	moveObject bs o = case o of
		Left l -> Left $ moveExplosion tick l
		Right r -> let j = hitShip bs r in
			maybe (Left $ newShipExplosion (expTime r) (pos r)) (\ok -> Right $ moveShip input tick ok) j

	allShips = rightsOf$ships g
	allExplosions = leftsOf$ships g
	allShots = rightsOf$shots g
	allSparks = leftsOf$shots g

	moveBullets = [ moveBullet tick b | b@(Bullet{loc=l}) <- bullets g, l `isPointInBox` (300*2,400*2) ]
	newBullets = either (\_->[]) (\p-> concat $ map (actShip p input tick) allShips) $ player g

	moveShot :: Either Explosion Bullet -> Either Explosion Bullet
	moveShot b = case b of
		Left l -> Left $ moveExplosion tick l
		Right r -> if hitBullet allShips r
			then Left $ newSparks (score g) (0,1,1) (loc r) --newShipExplosion (400/1000) (loc r)  hehe
			else Right $ moveBullet tick r
	newShots = [ Right b | b <- either (\_->[]) (\p-> actShip p input tick p) $ player g ]

gameRender g input = do
	drawStage $ stage g

	either drawShipExplosion drawShip $ player g
	mapM_ (either drawShipExplosion drawShip) $ ships g

	drawShots [ s | Right s <- shots g ] 
	mapM_ drawSparkExplosion [ s | Left s <- shots g ]

	drawBullets (abs $ bulletFlash g) $ bullets g

	color3 0 1 1
	Draw.textAt (-200,-300) $ "shots: " ++ (show $ length $ shots g)
	color3 1 0 1
	Draw.textAt (-200,-320) $ "bullets: " ++ (show $ length $ bullets g)
	--color3 1 1 0
	--renderTextAt (-100,-200) ("stageTime: " ++ (show $ stageTime g))

	color3 1 1 1
	let seed = (score g * 10) + continuesUsed g
	Draw.textAt (-300,360) $ "score: " ++ (show seed)
	Draw.textAt (-300,340) $ "lives: " ++ (show $ lives g)
	--return seed


gameListen g = return ()
