module ShmupTest.Stage where

import Nova.HumaMath (Scalar)

import ShmupTest.Ship

data Stage = Stage1 {
	}
	| Stage2 | Stage3 | Stage4 | Stage5
	deriving (Show,Read)

{-
class Opera s where
	precache :: s -> IO s
	shipsForStageTime :: s -> GLfloat -> [Ship]
	update :: s -> GLfloat -> s
	draw :: s -> IO ()

instance Opera Stage1 where
-}

newStage1 = Stage1

shipsForStageTime :: Stage -> Scalar -> [Ship]
shipsForStageTime Stage1 t
	| t > 200 = [
		newDrone (-100,100),
		newDrone (100,100)
		]
	| t > 100 = [
		newDrone (-120,190),
		newAutoDrone (120,190),
		newDrone (-80,210),
		newAutoDrone (80,210)
		]
	| otherwise = [newAutoDrone (-100,200), newAutoDrone (100,200)]

drawStage :: Stage -> IO ()
drawStage Stage1 = return ()
