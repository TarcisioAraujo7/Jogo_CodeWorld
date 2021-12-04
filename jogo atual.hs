{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
main = activityOf mundoInicial atualiza apresenta

data Giro = AntiHorario | Horario | SemGiro
            deriving (Show, Eq)

data Mundo = Mundo { posNave1, posNave2 :: Point,       --Posição das naves
                   velNave1, velNave2 :: Vector,        --Velocidade das naves
                   dirNave1, dirNave2 :: Double,        --Direção das naves
                   girNave1, girNave2 :: Giro,          --Giro das naves
                   accNave1, accNave2 :: Bool,          --Aceleração das naves
                   attNave1, attNave2 :: Bool,          --Estado da arma das naves
                   balas1, balas2 :: [Bala],            --Tiros da nave
                   clock1, clock2 :: Double,            --Velocidade de tiro das naves
                   asteroide1, asteroide2 :: [Point]}
                   deriving Show

data Bala = Bala { posBala :: (Double,Double),          --Caracteristicas dos projeteis
                   velBala :: Vector,
                   dirBala :: Double}
                   deriving Show
              
mundoInicial =                                          --Estado inicial do mundo
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
            balas1 = [],
            balas2 = [],
            clock1 = 0,
            clock2 = 0,
            asteroide1 =  [(-2,0),(-1,-1),(2, 0),(2,2), (-1,1)],
            asteroide2 = [(1,1), (2,0),(0,-2.5),(-1,-1),(-0.5,2.5)]
         }

balaPronta ::  Bala -> Picture                              --Picture do projetil
balaPronta  Bala{posBala=(x, y)} = balasNave
  where
    balasNave = translated x y $  solidCircle 0.15

navePronta ::  Picture                                      --Picture da nave
navePronta = translated (-x) (-y) (solidPolygon navePol)
  where
    navePol = [(0,0.7),(0,-0.7),(2.5, 0)]
    (x, y) = centroide navePol

ast1 :: [Point] -> Picture                                  --Picture do asteroide 1
ast1 list = colored brown (solidPolygon list)

ast2 :: [Point] -> Picture                                  --Picture do asteroide 2e
ast2 list = colored grey (solidPolygon list)

vRotacao = pi/4
mAceleracao = 2
vlSaida = (0,0)

unitVector :: Double -> Vector
unitVector a = (cos a, sin a)


type Segmento = (Point, Point)
type Poligono = [Point]

segmentosDoPol :: Poligono -> [Segmento]
segmentosDoPol pol = zip pol (tail pol ++ [head pol])

-- Área de polígono regular

areaPoligonoR :: Poligono -> Double
areaPoligonoR ps = 1/2 * ( sum ( map calc (zip ps (tail ps ++ [head ps])) ) ) 

calc :: (Point,Point) -> Double 
calc (p1,p2) = fst p1 * snd p2 -fst p2 * snd p1

-- Centróide de um polígono regular

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

---------------------------------------------------------------------------------------------------------------------

atualiza :: Event -> Mundo -> Mundo

atualiza (KeyPress "E") nave = nave {attNave1 = True}             --Funções que alteram a nave 1
atualiza (KeyRelease "E") nave = nave {attNave1 = False}
atualiza (KeyPress "D") nave = nave {girNave1 = Horario}
atualiza (KeyRelease "D") nave = nave {girNave1 = SemGiro}
atualiza (KeyPress "A") nave = nave {girNave1 = AntiHorario}
atualiza (KeyRelease "A") nave = nave {girNave1 = SemGiro}
atualiza (KeyPress "W") nave = nave {accNave1 = True}
atualiza (KeyRelease "W") nave = nave {accNave1 = False}

atualiza (KeyPress "L") nave = nave {attNave2 = True}             --Funções que alteram a nave 2
atualiza (KeyRelease "L") nave = nave {attNave2 = False}
atualiza (KeyPress "Right") nave = nave {girNave2 = Horario}
atualiza (KeyRelease "Right") nave = nave {girNave2 = SemGiro}
atualiza (KeyPress "Left") nave = nave {girNave2 = AntiHorario}
atualiza (KeyRelease "Left") nave = nave {girNave2 = SemGiro}
atualiza (KeyPress "Up") nave = nave {accNave2 = True}
atualiza (KeyRelease "Up") nave = nave {accNave2 = False}

atualiza (TimePassing t) nave = limites (rotaciona t (aplicaMov t (atirando t ( movimentacao t nave))))
atualiza _ nave = nave
   
---------------------------------------------------------------------------------------------------------------------

--Função para movimentar as naves
movimentacao t mundo@Mundo {posNave1 = p1, velNave1 = v1,dirNave1 = d1, girNave1 = g1, accNave1 = a1,
                            posNave2 = p2, velNave2 = v2, dirNave2 = d2, girNave2 = g2, accNave2 = a2} =
             mundo{posNave1 = mruvPos p1 v1 acc1 t, velNave1 = mruvVel v1 acc1 t, dirNave1 = mcuAng d1 va1 t,
                   posNave2 = mruvPos p2 v2 acc2 t, velNave2 = mruvVel v2 acc2 t, dirNave2 = mcuAng d2 va2 t}
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

--Função para rotacionar os asteroides
rotaciona t mundo@Mundo { asteroide1 = aste1,
                         asteroide2 = aste2} = mundo{asteroide1 =  map (rotatedPoint ((pi/2)*t)) aste1,
                                                     asteroide2 =  map (rotatedPoint ((pi/3)*t)) aste2}

--Funções para atirar
atirando t mundo@Mundo { balas1 = bs1, balas2 = bs2} = mundo{ balas1 = nbs1, clock1 = nclock1, balas2 = nbs2, clock2 = nclock2}
                 where ( nbs1, nclock1) = atirar1 t bs1 mundo
                       ( nbs2, nclock2) = atirar2 t bs2 mundo
                       
atirar1 :: Double -> [Bala] -> Mundo -> ([Bala], Double)
atirar1 t bs1 mundo@Mundo { clock1 = cl1, attNave1 = False} = (bs1,cl1)
atirar1 t bs1 mundo@Mundo {posNave1 = x,
                           velNave1 = v1,
                           dirNave1 = d1,
                           girNave1 = g1,
                           clock1 = cl1,
                           attNave1 = True,
                           accNave1 = a1} = if cl1 + t >= 1/6
                                      then (novaBala : bs1, cl1 + t - 1/6)
                                      else (bs1,  cl1 + t)
                                      where novaBala = Bala{posBala = x, velBala =  v1 , dirBala = d1}  
                                       
atirar2 :: Double -> [Bala] -> Mundo -> ([Bala], Double)
atirar2 t bs2 mundo@Mundo { clock2 = cl2, attNave2 = False} = (bs2,cl2)
atirar2 t bs2 mundo@Mundo {posNave2 = x,
                           velNave2 = v2,
                           dirNave2 = d2,
                           girNave1 = g2,
                           clock2 = cl2,
                           attNave2 = True,
                           accNave2 = a2} = if cl2 + t >= 1/6
                                      then (novaBala : bs2, cl2 + t - 1/6)
                                      else (bs2,  cl2 + t)
                                      where novaBala = Bala{posBala = x, velBala =  v2 , dirBala = d2}    
                                      
aplicaMov :: Double -> Mundo -> Mundo
aplicaMov t mundo@Mundo {balas1 = bs1,
                         balas2 = bs2} = mundo{balas1 = map (movProj t ) bs1,
                                               balas2 = map (movProj t ) bs2}
                                                   
movProj :: Double -> Bala -> Bala
movProj t bala@Bala {posBala = p,
                     velBala = v, 
                     dirBala = d1} = bala {posBala = patt, velBala = vatt}
                                    where patt = vectorSum (mruvPosBala p v (0, 0) t) (unitVector d1) 
                                          vatt = mruvVel v (0,0) t

--Função para limitar as balas para o mundo visivel
limites :: Mundo -> Mundo
limites mundo@Mundo {balas1 = b1, balas2 = b2} = mundo{balas1 = filter (checaLimites) b1, balas2 = filter (checaLimites) b2}

checaLimites :: Bala -> Bool
checaLimites bala@Bala {posBala = (xB, yB)} =  xB < 10 || xB > -10 || yB < 10 || yB > -10

---------------------------------------------------------------------------------------------------------------------------------------------------------                
type Velocidade = Vector
type Aceleracao = Vector
type Direcao = Double     --Ângulo em radianos


mruvPos :: Point -> Velocidade ->  Aceleracao -> Double -> Point    
mruvPos p v acc t =
  vectorSum p  (vectorSum (scaledVector t v)
                          (scaledVector (1/2 * t^2) acc))
                          
mruvPosBala :: Point -> Velocidade ->  Aceleracao -> Double -> Point    
mruvPosBala p v acc t =
  vectorSum p  (vectorSum (scaledVector (1/4 * t) v) (scaledVector (1/4 * t) acc))                          

mruvVel :: Velocidade -> Aceleracao -> Double -> Vector
mruvVel v acc t = vectorSum v (scaledVector t acc)

mcuAng :: Direcao -> Double -> Double -> Direcao
mcuAng d va t = d + va * t
----------------------------------------------------------------------------------------------------------------------------------------------------------
--Função de apresentação
apresenta :: Mundo -> Picture                                                                                                                                                                   
apresenta mundo@Mundo {posNave1 = (x1,y1),
                       dirNave1 = dir1,
                       posNave2 = (x2,y2),
                       dirNave2 = dir2,
                       balas1 = b1,
                       balas2 = b2,
                       asteroide1 = aste1,
                       asteroide2 = aste2} = translated x1 y1 (rotated dir1 navePronta) & 
                                             translated x2 y2 (rotated dir2 navePronta) &
                                             (pictures $ (map (balaPronta) b1)) &
                                             (pictures $ (map (balaPronta) b2)) &
                                             translated (-5) 5 (ast1 aste1) &  
                                             translated 2 (-3.5) (ast2 aste2)



























