module DUStatus where

data Status = Status {
	stHP :: Int,
	stMP :: Int,

	stDEX :: Int,
	stAGI :: Int,
	stCON :: Int,
	stWIS :: Int
	}
	deriving (Show,Read,Eq)

addStatus :: Status -> Status -> Status
addStatus a b = Status {
	stHP = stHP a + stHP b,
	stMP = stMP a + stMP b,
	stDEX = stAGI a + stDEX b,
	stAGI = stAGI a + stAGI b,
	stCON = stCON a + stCON b,
	stWIS = stWIS a + stWIS b
	}

emptyStatus = Status { stHP=0, stMP=0, stDEX=0, stAGI=0, stCON=0, stWIS=0 }

data BattleStatus = BattleStatus {
	-- dexterity
	bsAttack,
	bsDefense,
	-- agility
	bsDodge,
	bsLuck,
	-- concentration
	bsHealth,
	bsMagic,
	-- wisdom
	bsMatk,
	bsMdef
	:: Int
	}
	deriving (Show,Read,Eq)

