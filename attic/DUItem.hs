module DUItem where

import DUStatus

data ItemInfo = ItemInfo {
	tmName :: String,
	tmDescription :: String,
	tmCost :: Int,
	tmWeight :: Int
	}
	deriving (Show,Read,Eq)

data EquipInfo = EquipInfo {
	statusMod :: Status,
	battleStatus :: BattleStatus
	}
	deriving (Show,Read,Eq)

data Item =
	Useable ItemInfo |
	Body ItemInfo EquipInfo |
	Head ItemInfo EquipInfo |
	Feet ItemInfo EquipInfo |
	Accessory ItemInfo EquipInfo |
	Weapon ItemInfo EquipInfo |
	Skill ItemInfo |
	Special ItemInfo
	deriving (Show,Read,Eq)
