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
krok = 0.0005


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
	time :: Float,
	xs :: [(Float, Float)],
	ustart :: Float} deriving Show

--	FUNKCJE
-- Funkcja przekształcająca świat o zadany krok czasowy korzystająca ze schematu Eulera (tendencja do generowania chaosu (Ec rosnąca))
advanceTime :: Float -> World -> World
advanceTime step (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l time xs ustart) = (World (Pendulum newtau1 newomega1) (Pendulum newtau2 newomega2) l newtime xs1 ustart)
	where
		newomega1 = omega1 + ((domega1 l tau1 omega1 tau2 omega2) * step)	
		newtau1   = tau1   + (omega1 * step)
		newomega2 = omega2 + ((domega2 l tau1 omega1 tau2 omega2) * step)
		newtau2   = tau2   + (omega2 * step)
		newtime	  = time + step
		xsadd	      = ((l * ((sin newtau1) + (sin newtau2))), ((-l) * ((cos newtau1) + (cos newtau2))))
		xs1	      = if ((time * 1000) == (fromInteger ((round (time * 1000))))) then (xs ++ [xsadd]) else xs

-- Funkcja przekształcająca świat o zadany krok czasowy korzystająca lekko ulepszonego schematu Eulera (tendencja zagasająca)
advancetimebetter :: Float -> World -> World
advancetimebetter step (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) la time xs ustart) = (World (Pendulum newtau1 newomega1) (Pendulum newtau2 newomega2) la newtime xs1 ustart)
	where
		newomega1temp = omega1 + ((domega1 la tau1 omega1 tau2 omega2) * step)	
		newtau1temp   = tau1   + (newomega1temp * step)
		newomega2temp = omega2 + ((domega2 la tau1 omega1 tau2 omega2) * step)
		newtau2temp   = tau2   + (newomega2temp * step)
		newomega1     = omega1 + ((domega1 la (0.5 * (tau1+newtau1temp)) (0.5 * (omega1 + newomega1temp)) (0.5 * (tau2 + newtau2temp)) (0.5 * (omega2 + newomega2temp))) * step)
		newtau1	      = tau1   + (newomega1 * step)
		newomega2     = omega2 + ((domega2 la (0.5 * (tau1+newtau1temp)) (0.5 * (omega1 + newomega1temp)) (0.5*(tau2 + newtau2temp)) (0.5 * (omega2 + newomega2temp))) * step)
		newtau2	      = tau2   + (newomega2 * step)
		newtime	      = time + step
		xsadd	      = ((la * ((sin newtau1) + (sin newtau2))), ((-la) * ((cos newtau1) + (cos newtau2))))
		xs1	      = if ((time * 1000) == (fromInteger ((round (time * 1000))))) then (xs ++ [xsadd]) else xs



advancetimebetter2 :: Float -> World -> World
advancetimebetter2 step (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) la time xs ustart) = (World (Pendulum newtau1 newomega1) (Pendulum newtau2 newomega2) la newtime xs1 ustart)
	where
		ka1	      = ((domega1 la tau1 omega1 tau2 omega2) * step)
		kb1	      = ((domega2 la tau1 omega1 tau2 omega2) * step)
		newomega1temp = omega1 + (0.5 * ka1)
		newomega2temp = omega2 + (0.5 * kb1)
		ka2	      = ((domega1 la tau1 newomega1temp tau2 newomega2temp)*step)
		kb2	      = ((domega2 la tau1 newomega1temp tau2 newomega2temp)*step)
		newomega1temp2= omega1 + (0.5 * ka2)
		newomega2temp2= omega2 + (0.5 * kb2)
		ka3	      = ((domega1 la tau1 newomega1temp2 tau2 newomega2temp2)*step)
		kb3	      = ((domega2 la tau1 newomega1temp2 tau2 newomega2temp2)*step)
		newomega1temp3= omega1 + ka3
		newomega2temp3= omega2 + kb3
		ka4	      = ((domega1 la tau1 newomega1temp3 tau2 newomega2temp3)*step)
		kb4	      = ((domega2 la tau1 newomega1temp3 tau2 newomega2temp3)*step)
		newtau1	      = tau1   + (newomega1 * step)
		newtau2	      = tau2   + (newomega2 * step)
		newtime	      = time + step
		newomega1     = omega1 + ((ka1 + (2*ka2) + (2*ka3) + ka4)/6.0)
		newomega2     = omega2 + ((kb1 + (2*kb2) + (2*kb3) + kb4)/6.0)
		xsadd	      = ((la * ((sin newtau1) + (sin newtau2))), ((-la) * ((cos newtau1) + (cos newtau2))))
		xs1	      = if (length xs > 20000) then (adder (tail xs)) else (adder xs)
		adder list    = (if (((round (time / krok)) `mod` 10) == 0) then (list ++ [xsadd]) else list)




-- Funkcja generująca model energetyczny świata (listę stanów energetycznych świata) na ilość zadanych kroków 
model :: Float -> (Float -> World -> World) -> World -> (World, [(Float, Float, Float)])
model n funkcja (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) la t xs ustart)
	| (n*krok*10) == fromInteger (round (n*krok*10))	= ((newWorld), (snd prev) ++ [(ek (l newWorld) (tau (pendulum1 newWorld)) (tau (pendulum2 newWorld)) (omega (pendulum1 newWorld)) (omega (pendulum2 newWorld)), u (l newWorld) (tau (pendulum1 newWorld)) (tau (pendulum2 newWorld)), n*krok)])
	| n <= 0					= ((World (Pendulum tau1 omega1) (Pendulum tau2 omega2) la t xs ustart), [])
	| otherwise 					= (newWorld, snd prev)
	where 
	prev = model (n-1) funkcja (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) la t xs ustart)
	newWorld = funkcja krok (fst prev)

-- 1,3 -0,3
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
draw (World (Pendulum tau1 omega1) (Pendulum tau2 omega2) l time xs ustart) = 
	pictures [Graphics.Gloss.Scale (skala) (skala) $ line [(0.0, 0.0), (x1, -y1), (x2, -y2)], 
	Translate (skala * x1) ((-skala) * y1) $ Color blue $ circleSolid 5,
	Translate (skala * x2) ((-skala) * y2) $ Color blue $ circleSolid 5,
	Translate (-620) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show time),
	Translate (-310) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show (kinetyczna / ustart)),
	Translate (0) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show (potencjalna / ustart)),
	Translate (310) (-300) $ Graphics.Gloss.Scale (0.25) (0.25) $ Text (show ((kinetyczna + potencjalna)/(ustart))),
	Graphics.Gloss.Scale (skala) (skala) $ Color blue $ line xs]
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
start f la tau1 tau2 = do
	forkProcess (simulate (InWindow "symulacja" (1280,720) (10,10)) white (round (1/krok)) (World (Pendulum tau1 0.0) (Pendulum tau2 0.0) la 0.0 [(la*((sin tau1)+(sin tau2)), (-la)*((cos tau1)+(cos tau2)))] (u la tau1 tau2)) draw (advance f))
	wykresy f la tau1 tau2


-- Funkcja rysująca wykresy
wykresy :: (Float -> World -> World) -> Float -> Float -> Float -> IO ()
wykresy f la tau1 tau2 = plotLists [] [[(t, ek/ustart) |  (ek, _, t) <- symulacja], [ (t, u/ustart) | (_, u,t) <- symulacja], [ (t, (u+ek)/ustart) | (ek, u, t) <- symulacja]]
	where 
	symulacja = snd (model (20/krok) f ((World (Pendulum tau1 0.0) (Pendulum tau2 0.0) la 0.0 [(la * ((sin tau1) + (sin tau2)), (-la)*((cos tau1) + (cos tau2)))] ustart)))
	ustart = u la tau1 tau2

main = do start advancetimebetter2 3 (pi + 0.01) (pi - 0.01)
