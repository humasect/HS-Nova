module Editor.UI (
    panelInput,controlAt,
    isCursorInPanel,
    Panel(..),Control(..),ControlKind(..),
    drawPanel,brightColor,darkColor,
    newControl
) where

import Graphics.UI.GLUT (($=),Modifiers(..),KeyState(..),Key(..),SpecialKey(..),MouseButton(..))
import Control.Monad.State.Lazy
import Char (isAlphaNum)

import Common
import Nova.VectorOps
import Nova.HumaMath (isPointInSquare)
import Nova.Draw
import qualified Nova.GL as GL

data Panel m = Panel {
    bounding :: (V2,V2),
    controls :: m [Control m],
    drawBack :: Drawing
    }

data ControlKind = Label | EditLabel | Button | SmallButton | ClickLabel

--data ControlState = On | Off | Disabled

data Control m =
    Control {
           kind :: ControlKind,
         square :: (V2,V2),
          state :: m Bool,
            fun :: m (),
           fun2 :: m (),
          label :: m String,
    changeLabel :: Maybe (String -> m ())
    }

newControl :: Monad m => ControlKind -> (V2,V2) -> String -> Control m
newControl k sq l = Control {
           kind = k,
         square = sq,
          state = return False,
            fun = return (),
           fun2 = return (),
          label = return l,
  --labelAttribs = (False,(8,8)),
    changeLabel = Nothing
    }

isOver :: Monad m => V2 -> Control m -> Bool
isOver v c = v `isPointInSquare` (square c)

typeIntoControl :: Monad m => Char -> Control m -> m ()
typeIntoControl x c =
    justM (\j-> label c >>= j . case x of
                                    '\DEL' -> delLastChar
                                    '\b'   -> delLastChar
                                    an     -> addEndChar an) $ changeLabel c
    where
    delLastChar  s = if not$null s then reverse $ tail $ reverse s else s
    addEndChar x s = if isAlphaNum x || any ((==) x) "-_./" then s ++ [x] else s

drawControl :: Monad m => Control m -> m Drawing
drawControl c = do
    st <- state c
    l  <- label c
  --let (border,off) = labelAttribs c

    let (a,b) = square c

    return $ draw $ 
        case kind c of
            Button -> [
                if st then darkColor else brightColor,
                TextAt (b + (8,8)) l,
                drawBevel st a b]
            SmallButton -> [
                if st then darkColor else brightColor,
                TextAt (b + (2,2)) l,
                drawBevel st a b]
            Label -> [
                if st then Color3 (1,1,1) else brightColor,
                TextAt b l]
            EditLabel -> [
                if st then Color3 (1,1,1) else brightColor,
                TextAt b l]
{-              blueColor
              --darkColor
                pushAttrib & LineBit
                lineWidth 2
                Draw.squareAt LineLoop a b
                popAttrib -}
            ClickLabel -> case st of
                True -> [
                    Color4 (1,1,1,0.5),
                    FillSquareAt a b,
                    Color3 (0,0,0),
                    TextAt b l]
                False -> [
                    Color3 (1,1,1),
                    TextAt b l]

controlAt :: Monad m => Panel m -> V2 -> m (Maybe (Control m))
p `controlAt` v =
  --cs <- mapM controls p
    controls p >>= \cs' -> 
    let cs = filter (\c -> v `isOver` c) cs' in
    if null$cs then return Nothing else return (Just $ head cs)

translateCursor :: Panel m -> V2 -> V2
translateCursor p (vx,vy) = let (_,(x,y)) = bounding p in (vx-x,vy-y) -- (vx-(w-x), vy-(h-y))

panelInput :: MonadIO m => V2 -> Key -> Modifiers -> Panel m -> m Bool
panelInput sc key modifiers p = do
    let v = p `translateCursor` sc
  --liftIO $ putStrLn $ show v

    c <- p `controlAt` v

    case key of
        Char '\t'               -> return False
        Char x                  -> justM (typeIntoControl x) c >> return True
        MouseButton LeftButton  -> justM fun c >> return True
        MouseButton RightButton -> justM fun2 c >> return True

        _                       -> return True

drawPanel :: Monad m => Panel m -> V2 -> m Drawing
drawPanel p sc = do
    let v = p `translateCursor` sc
    let (a@(w,h),b@(x,y)) = bounding p

    -- draw backing of the panel
    let bk = draw [
                        Color4 (0,0,0,0.66),
                        FillSquareAt a b,
                        PushMatrix,
                        Translate b         ]

    cs <- controls p >>= mapM drawControl
    c  <- p `controlAt` v

    -- draw current control with blue highlight
    let blue c = draw $ case c of
                   Just j -> let ((cw,ch),(cx,cy)) = square j in [blueColor, FillSquareAt (cw+2,ch+2) (cx-2,cy-2)]
                   _      -> []


    return $ List $ bk : (cs ++ (blue c : [drawBack p, PopMatrix]))

isCursorInPanel :: V2 -> Panel m -> Bool
v `isCursorInPanel` p = v `isPointInSquare` (bounding p) --let (a,b) = bounding p in (v - b) `isPointInBox` a

------------

brightColor = Color3 (0.66,0.66,0.2)
darkColor   = Color3 (0.22,0.22,0.2)
blueColor   = Color4 (0   ,0.2 ,1  ,0.5)

{-
stateLabelColor :: ControlState -> IO ()
stateLabelColor On = color3 1 1 1
stateLabelColor Off = color3 1 1 1
stateLabelColor Disabled = darkColor
-}

drawBevel :: Bool -> V2 -> V2 -> Drawing
drawBevel down (w,h) (x,y) =
    let (c1,c2) = if down then (brightColor,darkColor) else (darkColor,brightColor) in List [
    LineWidth 2,

    c1,
    Begin GL.Lines,
    draw $ map Vertex2 [ (x,y),(x+w,y),
                         (x+w,y),(x+w,y+h) ],
    End,

    c2,
    Begin GL.Lines,
    draw $ map Vertex2 [ (x+w,y+h),(x,y+h),
                         (x,y+h),(x,y)      ],
    End,

    LineWidth 1]
