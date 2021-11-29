{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main = activityOf mundoInicial atualiza apresenta

data Giro = AntiHorario | Horario | SemGiro
            deriving (Show, Eq)

data Mundo = Mundo { posNave1, posNave2 :: Point, 
                   velNave1, velNave2 :: Vector,
                   dirNave1, dirNave2 :: Double,
                   girNave1, girNave2 :: Giro,
                   accNave1, accNave2 :: Bool,
                   attNave1, attNave2 :: Bool,
                   resistencia :: Int }
                   deriving Show
                   
mundoInicial =
    Mundo { posNave1 = (5,5),
            posNave2 = (-5,-5),
            velNave1 = (0,0),
            velNave2 = (0,0),
            girNave1 = SemGiro,
            girNave2 = SemGiro,
            dirNave1 = -pi/2,
            dirNave2 = pi/2,
            accNave1 = False,
            accNave2 = False,
            attNave1 = False,
            attNave2 = False,
            resistencia = 10
         }
         
navePronta :: Picture
navePronta = translated (-x) (-y) (solidPolygon navePol)
  where
    navePol = [(0,0.7),(0,-0.7),(2.5, 0)]
    (x, y) = centroide navePol
 
vRotacao = pi/4
mAceleracao = 2

unitVector :: Double -> Vector
unitVector a = (cos a, sin a)


type Segmento = (Point, Point)
type Poligono = [Point]

segmentosDoPol :: Poligono -> [Segmento]
segmentosDoPol pol = zip pol (tail pol ++ [head pol])

-- 4 Área de polígono regular
-- A = 1/2 Sum(i=0, i=n-1, x_i y_{i+1} - x_{i+1} y_i)
-- Se os pontos estão listados anti-horariamente A será positivo, senão será negativo.

areaPoligonoR :: Poligono -> Double
areaPoligonoR ps = 1/2 * ( sum ( map calc (zip ps (tail ps ++ [head ps])) ) ) 

calc :: (Point,Point) -> Double 
calc (p1,p2) = fst p1 * snd p2 -fst p2 * snd p1

-- 5 Centróide de um polígono regular
-- Centródie

centroide :: Poligono -> Point
centroide pol = (cx, cy)
  where
    cx = 1 / (6 * area) * sum (map calc1 segmentos) 
    cy = 1 / (6 * area) * sum (map calc2 segmentos) 
    segmentos = segmentosDoPol pol
    area = areaPoligonoR pol
    
calc1 :: ((Double,Double), (Double,Double)) -> Double
calc1 ((x1,y1), (x2,y2)) = (x1 + x2) * (x1*y2 - x2*y1)

calc2 :: ((Double,Double), (Double,Double)) -> Double
calc2 ((x1,y1), (x2,y2)) = (y1 + y2) * (x1*y2 - x2*y1)
-----------------------------------

atualiza :: Event -> Mundo -> Mundo

atualiza (KeyPress "Backspace") nave = nave {attNave1 = True}
atualiza (KeyRelease "Backspace") nave = nave {attNave1 = False}
atualiza (KeyPress "D") nave = nave {girNave1 = Horario}
atualiza (KeyRelease "D") nave = nave {girNave1 = SemGiro}
atualiza (KeyPress "A") nave = nave {girNave1 = AntiHorario}
atualiza (KeyRelease "A") nave = nave {girNave1 = SemGiro}
atualiza (KeyPress "W") nave = nave {accNave1 = True}
atualiza (KeyRelease "W") nave = nave {accNave1 = False}

atualiza (KeyPress "Ctrl") nave = nave {attNave2 = True}
atualiza (KeyRelease "Ctrl") nave = nave {attNave2 = False}
atualiza (KeyPress "Right") nave = nave {girNave2 = Horario}
atualiza (KeyRelease "Right") nave = nave {girNave2 = SemGiro}
atualiza (KeyPress "Left") nave = nave {girNave2 = AntiHorario}
atualiza (KeyRelease "Left") nave = nave {girNave2 = SemGiro}
atualiza (KeyPress "Up") nave = nave {accNave2 = True}
atualiza (KeyRelease "Up") nave = nave {accNave2 = False}

atualiza (TimePassing t) nave = movimentacao t nave
atualiza _ nave = nave
   

movimentacao t mundo@Mundo {posNave1 = p1, velNave1 = v1, dirNave1 = d1, girNave1 = g1, accNave1 = a1, posNave2 = p2, velNave2 = v2, dirNave2 = d2, girNave2 = g2, accNave2 = a2} =
             mundo{posNave1 = mruvPos p1 v1 acc1 t, velNave1 = mruvVel v1 acc1 t, dirNave1 = mcuAng d1 va1 t, posNave2 = mruvPos p2 v2 acc2 t, velNave2 = mruvVel v2 acc2 t, dirNave2 = mcuAng d2 va2 t}
    where acc1 = if a1
               then scaledVector mAceleracao (unitVector d1)
               else (0, 0) 
          acc2 = if a2
               then scaledVector mAceleracao (unitVector d2)
               else (0, 0)      
          va1
            | g1 == AntiHorario  = vRotacao
            | g1 == Horario  = -vRotacao
            | otherwise = 0 
          va2
            | g2 == AntiHorario  = vRotacao
            | g2 == Horario  = -vRotacao
            | otherwise = 0 
        
type Velocidade = Vector
type Aceleracao = Vector
type Direcao = Double     -- ângulo em radianos


mruvPos :: Point -> Velocidade ->  Aceleracao -> Double -> Point    
mruvPos p v acc t =
  vectorSum p  (vectorSum (scaledVector t v)
                          (scaledVector (1/2 * t^2) acc))

mruvVel :: Velocidade -> Aceleracao -> Double -> Vector
mruvVel v acc t = vectorSum v (scaledVector t acc)

mcuAng :: Direcao -> Double -> Double -> Direcao
mcuAng d va t = d + va * t

apresenta :: Mundo -> Picture 
apresenta nave@Mundo {posNave1 = (x1,y1), dirNave1 = dir1, posNave2 = (x2,y2), dirNave2 = dir2 } = translated x1 y1 (rotated dir1 navePronta) & translated x2 y2 (rotated dir2 navePronta)




























