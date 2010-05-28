module GrenadierBleu.CelestialBody where

import Nova.GL
import Nova.HumaMath
import qualified Nova.Draw as Draw

--import GrenadierBleu.Universe

data CelestialBody = CelestialBody {
	cbKind :: Either NaturalBody ArtificialBody,
	cbOrbits :: [Orbit],
	cbSpeed :: GLfloat,
	cbRotation :: GLfloat
	}
	deriving (Show,Read)

data Orbit = Orbit {
	orType :: OrbitType,
	orEllipse :: Ellipse,
	orBodies :: [(GLfloat,CelestialBody)]
	}
	deriving (Show,Read)

data OrbitType = NormalOrbit | AsteroidOrbit | IceOrbit | GasOrbit
	deriving (Show,Read)

data NaturalBody = Center | Star | Planet | Moon
	deriving (Show,Read)

data ArtificialBody = SpaceStation
	deriving (Show,Read)

emptyCelestial = CelestialBody {
	cbKind = Left Center,
	cbOrbits = [],
	cbSpeed = 1,
	cbRotation = 1
	}

instance GameObject CelestialBody where
	draw = drawCelestialBody (0,0)
	move tick cb = cb { cbOrbits = map moveOrbit $ cbOrbits cb }
		where moveOrbit o = o { orBodies = map (\(t,cb) -> (t+((1/cbSpeed cb)*tick), move tick cb)) $ orBodies o }

drawCelestialBody :: V2 -> CelestialBody -> IO ()
v `drawCelestialBody` cb = do
	color3 1 0 0
	Draw.lineLoop $ circlePoints 16 (v,1)
	--renderCircle 16 1 v
	Draw.cross (v,1)
	mapM_ (\o-> do
		color3 1 1 1
		Draw.lineLoop $ ellipsePoints 64 (orEllipse o) v
		mapM_ (\(t,b) -> (v <+> ellipse (orEllipse o) t) `drawCelestialBody` b) $ orBodies o
		) $ cbOrbits cb

bodiesAtPoint :: CelestialBody -> V2 -> [CelestialBody]
bodiesAtPoint cb v = let os = orbitsAtPoint cb v in []

orbitsAtPoint :: CelestialBody -> V2 -> [Orbit]
orbitsAtPoint cb v = []

--------

testCelestial = CelestialBody {
	cbKind = Left Center,
	cbOrbits = [
		Orbit { orType = NormalOrbit, orEllipse = ((30,30), 22), orBodies = [morebits2] },
		Orbit { orType = NormalOrbit, orEllipse = ((40,15), 0), orBodies = [
			(0.2, CelestialBody {
				cbKind = Left Star,
				cbOrbits = [
					Orbit { orType = NormalOrbit, orEllipse = ((10,8), 99), orBodies = [morebits] },
					Orbit { orType = NormalOrbit, orEllipse = ((13,4), 13), orBodies = [] }
					],
				cbSpeed = 10,
				cbRotation = 1
				}) -- , morebits
			 ] }
		],
	cbSpeed = 1,
	cbRotation = 1
	}
	where
	morebits = 
		(0.5, CelestialBody { cbKind = Left Star,
			cbOrbits = [ Orbit { orType = NormalOrbit, orEllipse = ((4,2), 103), orBodies = [] },
						Orbit { orType = NormalOrbit, orEllipse = ((2,6), 3), orBodies = [] }],
			cbSpeed = 6,
			cbRotation = 1})
	morebits2 = 
		(0.8, CelestialBody { cbKind = Left Star,
			cbOrbits = [ Orbit { orType = NormalOrbit, orEllipse = ((12,20), 10), orBodies = [bb] } ],
			cbSpeed = 23,
			cbRotation = 1})

	bb =
		(0, CelestialBody { cbKind = Left Star,
			cbOrbits = [ Orbit { orType = NormalOrbit, orEllipse = ((6, 6), 12), orBodies = [] } ],
			cbSpeed = 10,
			cbRotation = 1})
