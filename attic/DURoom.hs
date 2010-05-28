module DURoom where

import System.Random

import DUItem
import DUMonster

--data RoomSize = Room1x1 | Room1x2 | Room2x1 | Room2x2  -- ew.

data Room = Room {
	rmItems :: [Item],
	rmMonsters :: [Monster],
	--rmSize :: RoomSize,

	rmTileRows :: [[Bool]]
	}
	deriving (Show,Read)

{-
data Room =
	Entrance RoomInfo |
	Save RoomInfo |
	Monster RoomInfo |
	Trap RoomInfo |
	Treasure RoomInfo
	deriving (Show,Read)
-}

testRoom = Room {
	rmItems = [],
	rmMonsters = [],
	--rmSize = Room1x1,
	rmTileRows = []
	}

{-
makeRoom :: IO Room
makeRoom = do
	numExits <- randomRIO (1,4) :: IO Int
	rows <- makeRows
	return $ Room {
		rmItems = [],
		rmMonsters = [],
		rmTileRows = rows
		}
	where
	makeRows = take 10 $ repeat makeColumns
	makeColumns = take 10 $ repeat (randomRIO (0,1))
-}
