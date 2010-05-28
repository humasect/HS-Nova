module GrenadierBleu.GrenadierBleu where

import Nova.Game
import Nova.Draw hiding (draw)
import qualified Nova.GL as GL
import Nova.HumaMath
import Config           (titleBarHeight)

import GrenadierBleu.Object
import GrenadierBleu.Ship

data GrenadierBleu = GrenadierBleu {
    gameInfo :: GameInfo,
    input :: Input,

    player :: Ship
    }
  deriving (Show,Read)

instance Gaming GrenadierBleu where
    newGame       = newGrenadierBleu
    getInfo    gb = gameInfo gb
    setInput gb i = gb { input = i }

    gUpdate = update
    gRender = render
    gListen = listen

newGrenadierBleu = GrenadierBleu {
    gameInfo = GameInfo {
                         identifier = "gblu",
                         gameWidth = 640,
                         gameHeight = 640,
                         gameBackground = Just (0,0,0,0),
                         camera = (0,0,1)
                        },
    input = noInput,
    player = emptyShip
    }

update g tick =
    g {
        player = (move tick (player g)) { shAngle = angleToCursor $ shPos (player g) }
    }
    where
    angleToCursor v =
        (toDegrees $ angleForPoints (shPos$player g) (cursor$input g)) - 90

render :: Game GrenadierBleu Drawing
render = get >>= \gb -> return $ List [
    Enable GL.ScissorTest,
    Scissor 0 0 640 (576+titleBarHeight),

    -- temp background
    Color4 (1,0.5,0,0.22),
    FillBoxAt (640,640) (-320,-320),

    CameraDo (camera$gameInfo gb) $ draw (player gb),

    Color3 (1,0.5,0),
    Crosshair (cursor$input gb,32),

    Scissor 0 (576+titleBarHeight) 640 64,
    drawStatus,
    Disable GL.ScissorTest
    ]
    where
    drawStatus =
        -- similar to EditUI.drawPanel
        let (a@(w,h),b@(x,y)) = ((640,64),(-320,320-64)) in
        --color4 1 0.5 0 0.33
        MatrixDo $ List [
            Translate (x,y),
            Color3 (1,1,1),
            TextAt (0,0) "EN 99",
            TextAt (0,16) "M 255"]

listen g = return ()
