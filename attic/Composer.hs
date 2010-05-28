module Nova.Composer (
) where

import Nova.Synth

import Nova.EditUI

data Music = Music {
	tracks :: [Track],
	effects :: [Effect],
	bpm :: Int
	}
	deriving (Show,Read)

data Track = Track {
	generator :: Generator
	}
	deriving (Show,Read)

----

data Composer = Composer {
	music :: Music,
	view :: ViewMode,
	synthPanel :: Bool
	}
	deriving (Show,Read)

data ViewMode = Machines | Tracker deriving (Show,Read)

init :: Bool -> IO ()
init ss = return ()

shutdown :: Bool -> IO ()
shutdown ss = return ()