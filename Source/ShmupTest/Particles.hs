module ShmupTest.Particles where

import System.Random
import Control.Monad (when)

import Nova.GL
import Nova.HumaMath
import qualified Nova.Draw as Draw
import Nova.Render (renderImage)

data Bullet
	= Shot {
		loc :: V2,
		speed :: Scalar,
		radius :: Scalar}   -- temp !
	| Bullet {
		loc :: V2,
		dir :: V2,
		radius :: Scalar }
	| LineBullet { }
	| Particle {
		loc :: V2,
		dir :: V2,
		radius :: Scalar,
		color :: V3 }
	deriving (Show,Read)

{-
data Explosion
	= ShipExplosion {
		origin :: V2,
		particles :: [Bullet],
		startLife :: GLfloat,
		life :: GLfloat }
	| Sparks {
		origin :: V2,
		particles :: [Bullet],
		startLife :: GLfloat,
		life :: GLfloat }
	deriving (Show,Read)
-}

data Explosion = Explosion {
		origin :: V2,
		particles :: [Bullet],
		life :: Scalar,
		age :: Scalar }
	deriving (Show,Read)

newShipExplosion :: Scalar -> V2 -> Explosion
newShipExplosion l v = Explosion {
	origin = v,
	particles = parts,
	life = l,
	age = 0
	} where
	parts = [
		Particle { loc=(0,0), dir=angle North 8.1, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle South 8.2, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle West 8.3, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle East 8.4, radius=100, color=(1,1,0) },

		Particle { loc=(0,0), dir=angle NorthEast 8.5, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle NorthWest 8.6, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle SouthEast 8.7, radius=100, color=(1,1,0) },
		Particle { loc=(0,0), dir=angle SouthWest 8.8, radius=100, color=(1,1,0) }
		]
	angle d x = (vectorForCompDir d) * vecOf (800)   -- x not used... try it =)

newSparks :: Int -> V3 -> V2 -> Explosion
newSparks seed col v = let gen = mkStdGen seed in Explosion {
	origin = v,
	particles = parts gen,
	life = 200/1000,
	age = 0
	} where
	parts gen = let (x,_) = randomR (0,1000) gen in if (x::Int)<500 then parts1 else parts2
	parts1 = [
		Particle { loc=(0,0), dir=angle North 8.1, radius=10, color=col },
		Particle { loc=(0,0), dir=angle South 8.2, radius=10, color=col },
		Particle { loc=(0,0), dir=angle West 8.3, radius=10, color=col },
		Particle { loc=(0,0), dir=angle East 8.4, radius=10, color=col }
		]
	parts2 = [
		Particle { loc=(0,0), dir=angle NorthEast 8.5, radius=10, color=col },
		Particle { loc=(0,0), dir=angle NorthWest 8.6, radius=10, color=col },
		Particle { loc=(0,0), dir=angle SouthEast 8.7, radius=10, color=col },
		Particle { loc=(0,0), dir=angle SouthWest 8.8, radius=10, color=col }
		]
	angle d x = (vectorForCompDir d) * vecOf (800)   -- x not used... try it =)

{-
	parts = take 25 $ iterate 
	r g = randomR (-1,1) g
	rv g = let
		(x,g') = r g
		(y,g'') = r g'
		in (normalOf (x,y),g'')
	next i (v:vs) g = if i<=0 then []
-}

moveBullet :: Scalar -> Bullet -> Bullet
moveBullet tick b = case b of
	Shot {loc=(x,y)} -> b {loc=(x,y+(speed b * tick))}
	_ -> b {loc=(loc b) + (dir b * vecOf tick)}

drawShots :: [Bullet] -> IO ()
drawShots bs = do
	let vs = [l | (Shot {loc=l}) <- bs]
	begin & Triangles
	color3 0 0.5 1
	mapM_ (shot 9 4) vs
	mapM_ (shot 9 (-6)) vs
	color3 0.5 1 1
	mapM_ (shot 5 6) vs
	mapM_ (shot 5 (-4)) vs
	end
	where shot w yo (x,y) = do
		vertex2 (x-w) (yo+(y-10))
		vertex2 (x+w) (yo+(y-10))
		vertex2 x (yo+(y+10))

drawMissiles bs = do
	let vs = [l | (Shot {loc=l}) <- bs]
	--missile test =)
	color4 1 1 1 1
	mapM_ (\v -> renderImage "missile" v) vs

drawBullets :: Scalar -> [Bullet] -> IO ()
drawBullets f bs = do
	color3 1 f 0
	mapM_ (\b-> pointSize (realToFrac $ radius b * 2) >> (begin & Points) >> vertex2v (loc b) >> end) bs

drawParticle :: Scalar -> Bullet -> IO ()
drawParticle ep b@(Particle {radius=r,color=(cr,cg,cb)}) = do
	--color3 (r / 100) (r / 100) 0
	let rx = r * (1-ep)

	when (rx*2 > 0) $ do
		color4 cr cg cb ((1-ep)*(1/3))
		pointSize (realToFrac $ rx*2)
		Draw.points [loc b]

	when (rx*(1/2) > 0) $ do
		color4 cr cg cb (1-ep)
		pointSize (realToFrac $ rx*(1/2))
		Draw.points [loc b]

drawShipExplosion e@(Explosion {origin=(x,y), particles=bs, life=l}) = do
	let ep = explosionPerc e
	pushMatrix
	translate x y 0
	rotate (ep*360) 0 0 1

	pushMatrix
	color4 1 0 0 (1-ep)
	Draw.boxAt Quads ((l*350)*(1-ep),(l*350)*(1-ep)) (0,0)
	popMatrix

	mapM_ (drawParticle ep) bs
	popMatrix

drawSparkExplosion e@(Explosion {origin=(x,y), particles=bs, life=l}) = do
	let ep = explosionPerc e
	pushMatrix
	translate x y 0
	--rotate (ep*360) 0 0 (-1)
	color4 1 0 1 (1-ep)
	mapM_ (drawParticle ep) bs
	popMatrix

explosionPerc :: Explosion -> Scalar
explosionPerc e = age e / life e

moveExplosion :: Scalar -> Explosion -> Explosion
moveExplosion tick e =
	e {
	age=(age e) + tick,
	particles=map (moveBullet tick) $ particles e
	}
