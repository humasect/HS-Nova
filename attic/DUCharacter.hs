module DUCharacter where

import DURoom
import DUItem
import DUStatus

data Character = Character {
	chName :: String,
	chStatus :: Status,
	chStatusMod :: Status,
	chExperience :: Int,

	chWeapon :: Maybe Item,
	chBody :: Maybe Item,
	chFeet :: Maybe Item,
	chAccessory :: (Maybe Item, Maybe Item),
	chHead :: Maybe Item,
	chSkill :: Maybe Item,

	chRoom :: Room
	}
	deriving (Show,Read)

emptyCharacter = Character {
	chName = "empty",
	chStatus = emptyStatus,
	chStatusMod = emptyStatus,
	chExperience = 0,

	chWeapon = Nothing,
	chBody = Nothing,
	chFeet = Nothing,
	chAccessory = (Nothing, Nothing),
	chHead = Nothing,
	chSkill = Nothing,

	chRoom = testRoom
	}

healChar :: Character -> (Int,Int) -> Character
healChar c@(Character {chStatus=st}) (h,m) =
	let
		bs = charBattleStatus c
	in
	c {
		chStatus = st {
			stHP = min (stHP st + h) (bsHealth bs),
			stMP = min (stMP st + m) (bsMagic bs)
		} 
	}

---------------

testCharacter = emptyCharacter {
	chName = "testPlayer",
	chStatus = testStatus,
	chExperience = 1,
	chRoom = testRoom
	}
	where
	testStatus = Status {
		stHP = 75,
		stMP = 22,
		stDEX = 9,
		stAGI = 9,
		stCON = 9,
		stWIS = 9
		}

--------------

{-
statusDiff :: Status -> Status -> Status
statusDiff a b = a {
	hp = hp a - hp b,
	mp = mp a - mp b,
	xp = xp a - xp b
	}

charStatusDiff (Character {current=a, total=b}) = statusDiff b a

xpToNext :: Character -> Int
xpToNext c = let (Status {xp=xp}) = charStatusDiff c in xp
-}

xpForLevel :: Int -> Int
xpForLevel 1 = 100
xpForLevel 2 = 225
xpForLevel 3 = 375
xpForLevel 4 = 575
xpForLevel 5 = 800

xpGain :: Character -> Int -> Character
xpGain c x = let c' = c {chExperience=chExperience c + x} in undefined

charLevel :: Character -> Int
charLevel c = 1

charBattleStatus :: Character -> BattleStatus
charBattleStatus c =
	let
		s = addStatus (chStatus c) (chStatusMod c)
		lv = charLevel c
		bs = BattleStatus {
			bsAttack = stDEX s,
			bsDefense = stDEX s,
			bsDodge = stAGI s,
			bsLuck = stAGI s,
			bsHealth = (lv * 100) + (stCON s * 5),
			bsMagic = (lv * 100) + (stCON s * 5),
			bsMatk = stWIS s,
			bsMdef = stWIS s
		}
		in
	-- add equips.
	bs

equipItem :: Item -> Character -> Character
equipItem i c = undefined