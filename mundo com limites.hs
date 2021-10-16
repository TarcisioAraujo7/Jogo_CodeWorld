{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

type Mundo = (X,Y,[Point], Inclinacao, Velocidade)
type Velocidade = Double
type X = Double 
type Y = Double
type Inclinacao = Double
main = activityOf mundoInicial update visualizacao 

mundoInicial :: Mundo
mundoInicial = ( 0, 0,[(0,2.5),(-1,-1.5),(1,-1.5)], 0 , 0)

update :: Event -> Mundo -> Mundo
update (KeyPress "Left") (posx,posy,[(x),(y),(z)], i, v)  = (posx,posy,[(x),(y),(z)], i + (pi / 6), v)
update (KeyPress "Right") (posx,posy,[(x),(y),(z)], i, v) = (posx,posy,[(x),(y),(z)], i + (11 * pi / 6), v) 
update (KeyPress "Up") (posx,posy,[(x),(y),(z)], i, v) = (posx,posy,[(x),(y),(z)], i, v + 1)
update (TimePassing t)  (posx,posy,[(x),(y),(z)], i, v)  
  | attx >= 10 || attx <= -10 ||  atty >= 10 || atty <= -10  = (posx,posy,[(0,0),(0,0),(0,0)], i, v)
  | otherwise =   ( posx - v * sin(i) * t, posy + v * cos(i) * t, [(x),(y),(z)], i, v) 
  where attx = posx + v * sin(i) * t
        atty = posy - v * cos(i) * t

update _ (posx,posy,[(x),(y),(z)], i, v) = (posx,posy,[(x),(y),(z)], i, v)

visualizacao :: Mundo -> Picture
visualizacao (posx,posy,[(x),(y),(z)], i, v) = translated posx posy (rotated i (solidPolygon [(x),(y),(z)]))