module GrenadierBleu.Ship where

import Nova.VectorOps
import Nova.Draw
--import Nova.Winding

import GrenadierBleu.Object

--data Thruster = Thruster V2 Bool V2 Scalar  -- pos, state, direction, power ... add Range? using triangle windings~
data Thruster = Thruster {
	thPos :: V2,
	thState :: Bool,
	thDir :: V2,
	thPower :: Scalar
	}
	deriving (Show,Read)

data Ship = Ship {
	shPos, shVel :: V2,
	shAngle, shAngVel :: Scalar,

	shThrusters :: [Thruster]
	} deriving (Show,Read)

emptyShip :: Ship
emptyShip = Ship {
	shPos = (0,-96),
	shVel = (0,0),
	shAngle = 0,
	shAngVel = 12,
	shThrusters = []
	}

{-
shipFromModel :: String -> IO Ship
shipFromModel s = do
	ps <- flaggedPartsForModel "GB/ship.nvm" "thruster"
	return $ emptyShip { shThrusters = map thrusterFromPart ps }
	where
	thrusterFromPart (Part {winding=w}) =
		let fc = firstCorner w in
		let sc = secondCorner w in
		Thruster {
			thPos = fc,
			thState = False,
			thDir = normalOf (fc - sc),
			thPower = 100
		}
-}

instance Object Ship where
	draw sh = NoDraw -- renderModel "GB/ship.nvm" (shPos sh) (shAngle sh)

	move tick sh@(Ship {shPos=p,shVel=v,shAngle=a,shAngVel=av}) =
		sh { shPos=(p + v * vecOf tick), shAngle=(a + (av*tick)) }

	thinkSpawn sh = []

