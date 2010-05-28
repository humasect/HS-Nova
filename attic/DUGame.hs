module DUGame where

import Interact
import HumaMath
import Render
import GL
import Resource

import DUCharacter

data DUGame = DUGame {
	char :: Character,
	room :: RModel
	}
	deriving (Show,Read)

instance Interactor DUGame where
	update = gameUpdate
	render = gameRender
	listen = gameListen

newGame = DUGame {
	char = emptyCharacter
	}

gameUpdate :: DUGame -> Input -> GLfloat -> DUGame
gameUpdate g input tick = g

gameRender :: DUGame -> IO ()
gameRender g = return ()

gameListen :: DUGame -> IO DUGame
gameListen g = return g
