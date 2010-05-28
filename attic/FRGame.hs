module FRGame where

import Interact
import Draw
import GL
import HumaMath
import Winding
import Resource

data FRGame = FRGame {
	pos :: V2, frogDest :: V2,
	letGo :: Bool,

	lives :: Int,
	counter :: Scalar
	} deriving (Show,Read)

newGame = FRGame {
	pos = (0,-240), frogDest = (0,-240),
	letGo = True,

	lives = 3,
	counter = 0
	}

instance Interactor FRGame where
	update = gameUpdate
	render = gameRender
	listen = gameListen

	resourceList g = ["FR/level.nvm", "FR/frog.nvm"]

frogSpots = [192, -96, 0, 96, 192]

gameUpdate g input tick =
	g {
		pos = moveFrog,
		frogDest = tellFrog,
		counter = if isMoving then (if (counter g) >= 1 then 0 else counter g + tick) else 0,
		letGo = not $ any (\t -> t input) [up,down,left,right,button1,button2,button3]
		} where
	--moveFrog = (pos g) <+> (frogDest g <*> (100/tick))
	moveFrog =
		let (a0,a1) = pos g in
		let (b0,b1) = frogDest g in
		let n = (linearLerp a0 b0 (counter g), linearLerp a1 b1 (counter g)) in
		if magnitudeOf (pos g <-> frogDest g) < 2 then frogDest g else n
	tellFrog = if letGo g && not isMoving
		then keepPointInBox (240-16,320-16) (pos g <+> (moveVector input <*> 32))
		else frogDest g
	--moving = let ((a,b),(c,d)) = (pos g, frogDest g) in (a-c)>(-0.01) || (b-d)>(-0.01)
	isMoving = magnitudeOf (pos g <-> frogDest g) > 0

gameRender (rm,g) = do
	drawModel rm (1,1,1,1) (safeFindModel rm "FR/level.nvm") (0,0)
	drawModel rm (1,1,1,1) (safeFindModel rm "FR/frog.nvm") $ pos g

gameListen (rm,g) =
	loadResources rm (resourceList g)
