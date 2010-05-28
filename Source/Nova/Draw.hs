{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Nova.Draw (
    Drawing(..),
    drawWhen,drawIf,

    Drawable(..),
    drawLines,

    composeDrawing,

    startFrame,endFrame,dirty
) where

import Prelude   hiding (lines)
import Data.Bits hiding (rotate)

import qualified Graphics.UI.GLUT as GLUT
import           Data.List             (sort)
import           Foreign.Marshal.Utils (with)
import           Data.Monoid

import Common
import Nova.VectorOps
import Nova.HumaMath
import Nova.Resource
import Nova.Part
import {-# SOURCE #-} Nova.Render
import Nova.Winding

import           Nova.GL hiding (Lines,Points,LineLoop,TriangleFan)
import qualified Nova.GL as GL

data Drawing =
      NoDraw | List [Drawing] | Repeat Int Drawing
-- state
    
    | PushMatrix       | PopMatrix      | Translate V2 | Scale V2 | Rotate Scalar
    | LineWidth Scalar | PointSize Scalar
    | Enable Constants | Disable Constants
    | Begin Constants  | End
    | Scissor GLint GLint GLsizei GLsizei
    
-- primitives
   
    | TexCoord2 V2 --  | TexCoord2 Scalar Scalar
    | Vertex2 V2   --  | Vertex2 Scalar Scalar
    | Color3 V3    --  | Color3 Scalar Scalar Scalar
    | Color4 V4    --  | Color4 Scalar Scalar Scalar Scalar

-- higher level state

    | MatrixDo Drawing    | TexturedDo GLuint Drawing | PrimitiveDo Constants Drawing
    | CameraDo V3 Drawing | SmallerDo V2 Scalar Drawing
    | CheckErrors String

-- higher level primitives
--    | Vertices [V2]    | FullVertices [(V2,V2,V4)]
--    | Points [V2]      | Lines [Line] --  | LineLoop [V2] | TriangleFan [V2]
    | LineBoxAt V2 V2  | FillBoxAt V2 V2 | LineSquareAt V2 V2 | FillSquareAt V2 V2
    | TextAt V2 String | PTextAt V2 String
    | Grid Scalar      | Crosshair Circle
    | LineBurst Int Circle | Circle Circle
    | Arrow V2 V2
    | ImageRepeated Scalar RImage V2

{-
    | RenderResource String V2
    | RenderImage String V2 | RenderModel String V2 Scalar
    | RenderTextured String Drawing | RenderProc Winding Procedural
-}
-- resources and contents

    | DrawWinding Constants Winding
    | RenderPart Part
    | RenderProc Procedural Part

-- editor stuff
    | ResPreview V2 (Maybe String) | ResMaking V2 (Maybe String)
--    | BeginDepthClip Part | EndDepthClip

  deriving (Show,Read)

instance Monoid Drawing where
    mempty                      = NoDraw
    (List a) `mappend` (List b) = List ( a ++ b )
    (List a) `mappend`       b  = List ( a ++[b])
    a        `mappend` (List b) = List ([a]++ b )
    a        `mappend`       b  = List  [a,   b]

class Drawable a where
    draw :: a -> Drawing

instance Drawable Drawing where
    draw = id

instance Drawable d => Drawable (Maybe d) where
    draw  Nothing = NoDraw
    draw (Just d) = draw d

instance Drawable d => Drawable [d] where
    draw [] = NoDraw
    draw as = List $ map draw as

instance Drawable d => Drawable (Either d d) where
    draw (Left  d) = draw d
    draw (Right d) = draw d

{-
instance Drawable Constants where
--    draw (bm,d) = List [Begin bm, draw d,End]
    draw GL.Lines       = Begin GL.Lines
    draw GL.LineLoop    = Begin GL.LineLoop
    draw GL.TriangleFan = Begin GL.TriangleFan
    draw GL.Quads       = Begin GL.Quads
    draw GL.Polygon     = Begin GL.Polygon
    draw GL.Points      = Begin GL.Points

    draw c = error $ "unDrawable constant: " ++ show c

------------

instance (Drawable d,Drawable e) => Drawable (d,e) where
    draw (a,b) = List [draw a,draw b]

instance (Drawable d,Drawable e,Drawable f) => Drawable (d,e,f) where
    draw (a,b,c) = List [draw a,draw b,draw c]
-}

instance Drawable V2 where
    draw = Vertex2

instance Drawable V4 where
    draw = Color4

--instance Drawable Line where
--    draw (a,b) = List [draw a,draw b]

drawLines :: [Line] -> Drawing
drawLines ls = List [Begin GL.Lines,List $ concatMap (\(a,b) -> [Vertex2 a,Vertex2 b]) ls,End]

instance Drawable Step where
    draw (Step t v c) = List [Color4 c, TexCoord2 t, Vertex2 v]

instance Drawable Part where
    draw p = RenderPart p

instance Drawable (Procedural,Part) where
    draw (pr,p) = RenderProc pr p

instance Drawable (Resource,V2,Scalar) where
    draw (r@(RImage {}),v,s) = ImageRepeated s r v
    draw (r@(RModel {}),v,s) = undefined --renderModel r v s
    draw _ = NoDraw

{-
drawMap :: Drawable d => (a -> d) -> [a] -> [d]
drawMap = draw.map

drawFor :: Drawable d => [a] -> (a -> d) -> [d]
drawFor = draw.for
-}

drawWhen :: Drawable d => Bool -> d -> Drawing
drawWhen True  d = draw d
drawWhen False _ = NoDraw

drawIf :: Drawable d => Bool -> d -> d -> Drawing
drawIf  True a _ = draw a
drawIf False _ b = draw b

composeDrawing :: Drawing -> IO ()
--composeDrawing c = putStrLn (show c) >> draw' c         -- debug drawing
--composeDrawing c = draw' c >> checkRenderErrors (show c)   -- debug GL
composeDrawing c = draw' c

draw' :: Drawing -> IO ()
draw'  NoDraw       = return ()
draw' (List [])     = return ()
draw' (List (c:cs)) = composeDrawing c >> (draw' $ List cs)
--draw' (Object   o)  = (draw.drawing) o
draw' (Repeat s d)
    | s > 0         = composeDrawing d >> composeDrawing (Repeat (s-1) d)
    | otherwise     = return ()

draw' (Enable e)          = enable  & e
draw' (Disable e)         = disable & e
draw'  PushMatrix         = pushMatrix
draw'  PopMatrix          = popMatrix
draw' (Translate (x,y))   = translate x y 0
draw' (Scale (x,y))       = scale     x y 0
draw' (Rotate a)          = rotate a  0 0 1
draw' (Begin pm)          = begin   & pm
draw'  End                = end
draw' (Scissor x y x2 y2) = scissor   x y x2 y2

draw' (TexCoord2  (u,v)) = texCoord2 u v
--draw' (TexCoord2    u v ) = texCoord2 u v
draw' (Vertex2    (x,y)) = vertex2   x y
--draw' (Vertex2      x y ) = vertex2   x y
draw' (Color3 (r,g,b)  ) = color3    r g b
--draw' (Color3   r g b   ) = color3    r g b
draw' (Color4 (r,g,b,a)) = color4    r g b a
--draw' (Color4   r g b a ) = color4    r g b a
draw' (PointSize     sz) = pointSize (realToFrac sz)
draw' (LineWidth     sz) = lineWidth (realToFrac sz)

draw' (MatrixDo       d)  = pushMatrix >>   composeDrawing d >> popMatrix
--draw (EnableDo e c) = enable & e >> cmd c >> disable & e
draw' (PrimitiveDo pm d)  = primitiveDo pm (composeDrawing d)
draw' (CameraDo     c d)  = cameraDo   c   (composeDrawing d)
draw' (TexturedDo   t d)  = texturedDo t   (composeDrawing d)
draw' (SmallerDo  c s d)  = smallerDo  c s (composeDrawing d)
draw' (CheckErrors    s)  = checkRenderErrors s

--draw' (Vertices       vs) = mapM_ vertex2v vs
--draw' (Lines          ls) = primitiveDo GL.Lines  (mapM_ (\(a,b)-> vertex2v a >> vertex2v b) ls)
--draw' (Points         vs) = primitiveDo GL.Points (mapM_           vertex2v                  vs)
draw' (LineBurst steps c) = lineBurst steps c
draw' (LineBoxAt     a b) = primitiveDo GL.LineLoop $ boxAt    a b
draw' (FillBoxAt     a b) = primitiveDo GL.Quads    $ boxAt    a b
draw' (LineSquareAt  a b) = primitiveDo GL.LineLoop $ squareAt a b
draw' (FillSquareAt  a b) = primitiveDo GL.Quads    $ squareAt a b
draw' (TextAt        v s) = textAt' v True  s
draw' (PTextAt       v s) = textAt' v False s
draw' (Grid           sz) = grid sz
draw' (Crosshair       c) = lineBurst 4 c >> primitiveDo GL.LineLoop (mapM_ vertex2v (c `plotCircleSteps` (0,32)))
draw' (Arrow         a b) = arrow a b

draw' (ImageRepeated s r v) = imageRepeated s r v

draw' (DrawWinding bm w) = primitiveDo bm $ mapM_ (\(Step t v c) -> color4v c >> texCoord2v t >> vertex2v v) w
draw' (RenderPart     p) = renderPart p
draw' (RenderProc  pr p) = renderProcedure pr p


{-
--draw' (RenderResource s v) = renderResource s v
draw' (RenderImage s v) = withResource s >>= (\(RImage w h t) -> let (w',h') = (fromIntegral w, fromIntegral h) in
    texturedDo t $ primitiveDo GL.Quads (boxAt (w'/2,h'/2) v))
draw' (RenderModel s v a) = renderModel s v a >>= draw
draw' (RenderTextured s d) = withResource s >>= \(RImage _ _ t) -> texturedDo t $ draw d
-}

primitiveDo :: Constants -> IO () -> IO ()
primitiveDo bm f = begin & bm >> f >> end

-- |Heart of 'textAt' and 'ptextAt' functions.
textAt' :: V2 -> Bool -> String -> IO ()
textAt' (x,y) mono s = do
    pushMatrix
    pushAttrib & LineBit
    lineWidth  2
    enable     & LineSmooth
    translate  x (y+2) 0    -- to align the GLUT font better.
    scale      0.08 0.08 0.08
    GLUT.renderString (if mono then GLUT.MonoRoman else GLUT.Roman) s
    popAttrib
    popMatrix

lineBurst :: Int -> Circle -> IO ()
lineBurst steps c@(v,_) = primitiveDo GL.Lines (mapM_ (\rv -> vertex2v v >> vertex2v rv) (c `plotCircleSteps` (0,steps)))

boxAt :: V2 -> V2 -> IO ()
boxAt (w,h) (x,y) = do
    texCoord2 0 0
    vertex2   ((-w)+x) ((-h)+y)
    texCoord2 1 0
    vertex2   (w+x)    ((-h)+y)
    texCoord2 1 1
    vertex2   (w+x)    (h+y)
    texCoord2 0 1
    vertex2   ((-w)+x) (h+y)

-- |Draw a @primitive@ sized @(w,h)@ at @pos@ with unit texture coordinates,
-- using width and height as extent from bottom left corner.
squareAt :: V2 -> V2 -> IO ()
squareAt (w,h) (x,y) = do
    texCoord2 0 0
    vertex2   x     y
    texCoord2 1 0
    vertex2   (x+w) y
    texCoord2 1 1
    vertex2   (x+w) (y+h)
    texCoord2 0 1
    vertex2   x     (y+h)

-- |Draw image where (0,0) is the center of the image.
image :: RImage -> V2 -> IO ()
image r@(RImage w h t) v =
             let (w',h') = (fromIntegral w, fromIntegral h) in
    texturedDo t $ primitiveDo GL.Quads (boxAt (w'/2,h'/2) v)

-- |Draw image where (0,0) is bottom left corner of image.
imageAtCorner :: RImage -> V2 -> IO ()
imageAtCorner r@(RImage w h t) v =
                     let (w',h') = (fromIntegral w, fromIntegral h) in
    texturedDo t $ primitiveDo GL.Quads (squareAt (w',h') v)

-- |Draw @n@ times repeated the @image@ at @pos@, where (0,0) is the center of the image*n.
imageRepeated :: Scalar -> Resource -> V2 -> IO ()
imageRepeated times r@(RImage w' h' t) (x,y) =
                                 let (w,h)   = (fromIntegral w', fromIntegral h') in
                                 let (wx,hy) = (w * times, h * times)             in

    texturedDo t $ do
        begin     & GL.Quads
        texCoord2 (((-wx)+x)/w) (((-hy)+y)/h)
        vertex2   ((-wx)+x)     ((-hy)+y)
        texCoord2 ((wx+x)/w)    (((-hy)+y)/h)
        vertex2   (wx+x)        ((-hy)+y)
        texCoord2 ((wx+x)/w)    ((hy+y)/h)
        vertex2   (wx+x)        (hy+y)
        texCoord2 (((-wx)+x)/w) ((hy+y)/h)
        vertex2   ((-wx)+x)     (hy+y)
        end

{-
-- |Draw image where (0,0) is bottom left corner of image.
imageAtCorner :: RImage -> V2 -> IO ()
imageAtCorner r@(RImage w h t) v =
    let (w',h') = (fromIntegral w, fromIntegral h) in
    texturedDo t $ primitiveDo GL.Quads (squareAt (w',h') v)
-}

-- |Enable and bind given @image@, perform @f@, then disable and unbind image.
texturedDo :: GLuint -> IO () -> IO ()
texturedDo t f = do
    enable       & Texture2D
    (bindTexture & Texture2D) t
    f
    (bindTexture & Texture2D) 0
    disable      & Texture2D

-- |Draw grid of size @sz@, with green for X and red for Y crossing at (0,0). Number of lines are 2048 / @sz@.
grid :: Scalar -> IO ()
grid sz = do
    let range = [-num..num] :: [Scalar]
    color3 0.25 0.25 0.25
    pushAttrib & LineBit
    lineWidth  1
    disable    & LineSmooth
    pushMatrix
    scale      sz sz sz
    primitiveDo GL.Lines $ mapM_ (\i -> rows i >> cols i) range
    lineWidth  2
    primitiveDo GL.Lines $ do
        color3 0.5 0 0
        vertex2v (-num,    0) >> vertex2v (num,   0)
        color3 0 0.5 0
        vertex2v (   0, -num) >> vertex2v (  0, num)
    popMatrix
    popAttrib
    where
    num    = 2048/sz
    rows i = vertex2v (-num,    i) >> vertex2v (num,   i)
    cols i = vertex2v (   i, -num) >> vertex2v (  i, num)

-- |At @pos@, under the scaling of @s@, perform @f@.
smallerDo :: V2 -> Scalar -> IO () -> IO ()
smallerDo (x,y) s f = do
    pushMatrix
    -- both of these have the same effect .. not sure what to do here, for translating+scaling.
    translate x y 0
    scale     s s 1
  --translate (x/s) (y/s) 0
    f
    popMatrix

-- |Under the camera @(cx,cy,scale)@, perform @f@.
cameraDo :: V3 -> IO () -> IO ()
cameraDo (cx,cy,cz) f = do
    pushMatrix
    translate ((-cx) * cz) ((-cy) * cz) 0
    scale        cz    cz               1
    f
    popMatrix

arrow :: V2 -> V2 -> IO ()
arrow a b@(ex,ey) = do
    let m = magnitudeOf (b - a)
    let ang = toDegrees $ angleForPoints a b

    checkRenderErrors "before"
    begin & GL.Lines
    vertex2v a
    vertex2v b
    end

    checkRenderErrors "mid"
    pushMatrix
    translate ex ey 0
    rotate (ang-15) 0 0 1
    begin & GL.Lines >> vertex2 0 0 >> vertex2 (-16) 0 >> end
    rotate 30 0 0 1
    begin & GL.Lines >> vertex2 0 0 >> vertex2 (-16) 0 >> end
    popMatrix
    
    checkRenderErrors "after"


-----------------------------------------------------------------------------------------------

-- |If any GL errors, report tagged with string.
checkRenderErrors :: String -> IO ()
checkRenderErrors n = GLUT.get GLUT.errors >>= mapM_ (\(GLUT.Error c s)-> error $ n ++ ": " ++ s)

-- |Prepares a frame for rendering into viewport @width@ @height@, clearing with color @(r,g,b)@,
-- sets up orthagonal matrix accordingly.
startFrame :: Scalar -> Scalar -> Maybe V4 -> IO ()
startFrame w h cc = do
    case cc of
      Just (r,g,b,a) -> do
        clearColor (realToFrac r) (realToFrac g) (realToFrac b) (realToFrac a)
        clearDepth 1
        depthMask  & True
        clear      $ (toConst ColorBufferBit) .|. (toConst DepthBufferBit) .|. (toConst StencilBufferBit)
        disable    & DepthTest
        depthMask  & False
      _ -> return ()

    matrixMode & Projection
    loadIdentity
    ortho      (-(w/2)) (w/2) (-(h/2)) (h/2) (-32) 32
    matrixMode & Modelview
    loadIdentity

    mapM_ (enable &) [LineSmooth,PointSmooth,Blend]
    blendFunc (toConst SourceAlpha) (toConst OneMinusSourceAlpha)

-- |Finishes the prepared frame. Swaps back buffer to front.
endFrame :: IO ()
endFrame = GLUT.swapBuffers

dirty :: IO ()
dirty = GLUT.postRedisplay Nothing
