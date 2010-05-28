module Nova.GL (
	vertex2,vertex2v,vertex3,color3,color4,color4v,translatef,scalef,texCoord2,texCoord2v,rotatef,
	prim,

	module Graphics.Rendering.OpenGL.GL
) where

import Prelude hiding (lines)

import Graphics.Rendering.OpenGL.GL hiding (get)

import Nova.HumaMath

texCoord2 :: Scalar -> Scalar -> IO ()
texCoord2 u v = texCoord $ TexCoord2 u v

texCoord2v :: V2 -> IO ()
texCoord2v (u,v) = texCoord $ TexCoord2 u v

vertex2v :: V2 -> IO ()
vertex2v (x,y) = vertex $ Vertex2 x y
vertex2 :: Scalar -> Scalar -> IO ()
vertex2 x y = vertex $ Vertex2 x y
vertex3 :: Scalar -> Scalar -> Scalar -> IO ()
vertex3 x y z = vertex $ Vertex3 x y z

color3 :: Scalar -> Scalar -> Scalar -> IO ()
color3 r g b = color $ Color3 r g b

color4 :: Scalar -> Scalar -> Scalar -> Scalar -> IO ()
color4 r g b a = color $ Color4 r g b a

color4v :: V4 -> IO ()
color4v (r,g,b,a) = color $ Color4 r g b a

translatef :: Scalar -> Scalar -> IO ()
translatef x y = translate $ Vector3 x y 0
scalef :: Scalar -> Scalar -> Scalar -> IO ()
scalef x y z = scale x y z
rotatef :: Scalar -> Scalar -> Scalar -> Scalar -> IO ()
rotatef a x y z = rotate a $ Vector3 x y z

prim :: PrimitiveMode -> [V2] -> IO ()
prim pm vs = unsafeRenderPrimitive pm $ mapM_ vertex2v vs

