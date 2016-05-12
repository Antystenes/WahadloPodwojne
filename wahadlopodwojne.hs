import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gnuplot.Simple
import System.Posix.Process

--	STAŁE
-- przyspieszenie ziemskie 
g :: Float
g = 9.81

-- krok czasowy do obliczen
krok :: Float
krok = 0.01

-- masa
masa :: Float
masa = 1.0

--	STRUKTURY DANYCH
data Pendulum = Pendulum { tau :: Float,
	omega :: Float
	} deriving Show

data World = World { pendulum1 :: Pendulum,
	pendulum2 :: Pendulum,
	l :: Float,
	time :: Float}

--	FUNKCJE
-- Funkcja przekształcająca świat o zadany krok czasowy korzystająca ze schematu Eulera (tendencja do generowania chaosu (Ec rosnąca))
advanceTime :: Float -> World -> World
advanceTime step (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l time) = (World (Pendulum newtau1 newomega1) (Pendulum newtau2 newomega2) l newtime)
	where
		newomega1 = omega1 + ((domega1 l tau1 omega1 tau2 omega2) * step)	
		newtau1   = tau1   + (omega1 * step)
		newomega2 = omega2 + ((domega2 l tau1 omega1 tau2 omega2) * step)
		newtau2   = tau2   + (omega2 * step)
		newtime	  = time + step

-- Funkcja przekształcająca świat o zadany krok czasowy korzystająca lekko ulepszonego schematu Eulera (tendencja zagasająca)
advancetimebetter :: Float -> World -> World
advancetimebetter step (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l time) = (World (Pendulum newtau1 newomega1) (Pendulum newtau2 newomega2) l newtime)
	where
		newomega1temp = omega1 + ((domega1 l tau1 omega1 tau2 omega2) * step)	
		newtau1temp   = tau1   + (newomega1temp * step)
		newomega2temp = omega2 + ((domega2 l tau1 omega1 tau2 omega2) * step)
		newtau2temp   = tau2   + (newomega2temp * step)
		newomega1     = omega1 + ((domega1 l (0.5 * (tau1+newtau1temp)) (0.5 * (omega1 + newomega1temp)) (0.5 * (tau2 + newtau2temp)) (0.5 * (omega2 + newomega2temp))) * step)
		newtau1	      = tau1   + (newomega1 * step)
		newomega2     = omega2 + ((domega2 l (0.5 * (tau1+newtau1temp)) (0.5 * (omega1 + newomega1temp)) (0.5*(tau2 + newtau2temp)) (0.5 * (omega2 + newomega2temp))) * step)
		newtau2	      = tau2   + (newomega2 * step)
		newtime	      = time + step

-- Funkcja generująca model świata (listę stanów świata) na ilość zadanych kroków 
model :: Float -> (Float -> World -> World) -> World -> [World]
model n funkcja x 
	| n >= 1	= prev ++ [(funkcja krok (last prev))]
	| otherwise	= [x]
	where prev = model (n-1) funkcja x

-- pochodne po czasie z omeg (prędkości kątowych)
domega1 :: Float -> Float -> Float -> Float -> Float -> Float
domega1 l tau1 omega1 tau2 omega2 = ( (3 * g * (sin tau1)) + (g * (sin(tau1 - (2*tau2)))) + (omega1*omega1*l*(sin(2 * (tau1 - tau2)))) + (2 * l * omega2 * omega2 *(sin (tau1 - tau2)))) / (l * ((cos (2 * (tau1 - tau2))-3)))

domega2 :: Float -> Float -> Float -> Float -> Float -> Float
domega2 l tau1 omega1 tau2 omega2 = -(2*(sin(tau1-tau2))*((2*g*(cos tau1))+(2*l*omega1*omega1)+(l*(cos (tau1 - tau2))*omega2*omega2) )) / (l * ((cos (2 * (tau1 - tau2))-3)))

--	ENERGIE
-- Funkcja energii kinetycznej układu
ek :: Float -> Float -> Float -> Float -> Float -> Float
ek l tau1 tau2 omega1 omega2 = 0.5 * l * l * masa * ((2 * omega1 * omega1) + (2 * (cos (tau1 - tau2))* omega1 * omega2) + omega2 * omega2)

-- Funkcja energii potencjalnej układu
u :: Float -> Float -> Float -> Float
u l tau1 tau2 = masa * g * l * (3 - (2 * (cos tau1)) - (cos tau2))

-- Funkcja rysujaca wahadlo na podstawie katow
draw :: World -> Picture 
draw (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l time) = 
	pictures [Graphics.Gloss.Scale (skala) (skala) $ line [(0.0, 0.0), (x1, -y1), (x2, -y2)], 
	Translate (skala * x1) ((-skala) * y1) $ Color blue $ circleSolid 5,
	Translate (skala * x2) ((-skala) * y2) $ Color blue $ circleSolid 5,
	Translate (-620) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show time),
	Translate (-310) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show kinetyczna),
	Translate (0) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show potencjalna),
	Translate (310) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show (kinetyczna + potencjalna))]
	where
	x1 = l * (sin tau1)
	y1 =l * (cos tau1)
	x2 = x1 + (l * (sin tau2))
	y2 = y1 + (l * (cos tau2))
	kinetyczna = ek l tau1 tau2 omega1 omega2
	potencjalna = u l tau1 tau2
	skala = 120 / l

-- Overlay dla "simulate"
advance :: (Float -> World -> World) -> ViewPort -> Float -> World -> World
advance f view step model = f step model	

-- Funkcja inicjująca 
start :: (Float -> World -> World) -> Float -> Float -> Float -> IO ()
start f l tau1 tau2 = do
	forkProcess(simulate (InWindow "symulacja" (1280,720) (10,10)) white 100 (World (Pendulum tau1 0.0) (Pendulum tau2 0.0) l 0.0) draw (advance f))
	wykresy f l tau1 tau2

-- Funkcja rysująca wykresy
wykresy :: (Float -> World -> World) -> Float -> Float -> Float -> IO ()
wykresy f l tau1 tau2 = plotLists [] [[(t, u l tau1 tau2) | (World (Pendulum tau1 _) (Pendulum tau2 _) l t) <- symulacja], [(t, ek l tau1 tau2 omega1 omega2) | (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l t) <- symulacja], [(t, (u l tau1 tau2)+(ek l tau1 tau2 omega1 omega2)) | (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l t) <- symulacja]]
	where symulacja = model (20/krok) f (World (Pendulum tau1 0.0) (Pendulum tau2 0.0) l 0.0)
