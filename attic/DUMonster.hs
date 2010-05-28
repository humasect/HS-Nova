module DUMonster where

import DUStatus
import DUItem

data Monster = Monster {
	msName :: String,
	msHP :: Int,
	msDrops :: [Item],
	msExperience :: Int,
	msBattleStatus :: BattleStatus
	}
	deriving (Show,Read)
