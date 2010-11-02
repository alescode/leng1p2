module Geometria (dibujarPoligono,
                  dibujarPoligonoRegular,
                  dibujarStrip) where

import Lienzo
import Utilidades
import Data.List

-- Traza lineas entre los puntos especificados en la lista de posiciones
dibujarLineas :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarLineas lienzo lista@(p1@(x1, y1):p2@(x2, y2):ps) c =
        dibujarLineas (lineaEntreDosPuntos lienzo p1 p2 c) (p2:ps) c
dibujarLineas lienzo _ _ = lienzo

-- Funciones para dibujar poligonos
obtenerPuntosPoligono :: Posicion -> Int -> Int -> [Posicion]
obtenerPuntosPoligono p numLados lado = 
    calcularPuntos centroPoligono radio anguloCentral anguloInicial numLados
     where anguloAlCentro = -360 / (2* (fromIntegral numLados))
           radio = round $ (fromIntegral lado) / (2 * (sin $ pi / (fromIntegral numLados)))
           centroPoligono = last . reverse $ obtenerLinea p anguloAlCentro radio
           anguloInicial = 90 - anguloAlCentro
           anguloCentral = 360 / (fromIntegral numLados)

calcularPuntos :: Posicion -> Int -> Float -> Float -> Int -> [Posicion]
calcularPuntos _ _ _ _ 0 = []
calcularPuntos centro radio anguloCentral angulo numLados = 
    (last . reverse $ obtenerLinea centro angulo radio) :
    (calcularPuntos centro radio anguloCentral (angulo + anguloCentral) (numLados-1))

dibujarPoligono :: Bool -> Lienzo -> [Posicion] -> Char -> Lienzo
dibujarPoligono contorno = if contorno then dibujarContorno
                           else dibujarPoligonoLibre

dibujarPoligonoLibre :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarPoligonoLibre lienzo lista@(p1@(x1, y1):p2@(x2, y2):ps) c =
    lineaEntreDosPuntos 
        (dibujarPoligonoLibre (lineaEntreDosPuntos lienzo p1 p2 c) (p2:ps) c) (last lista) p1 c  
dibujarPoligonoLibre lienzo _ _ = lienzo

dibujarContorno :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarContorno lienzo [] _ = lienzo
dibujarContorno lienzo lista c =
    lineaEntreDosPuntos (dibujarLineas lienzo listaPorAngulo c) (last listaPorAngulo) p0 c
     where listaOrdenada = sortBy (ordenTuplas) lista
           p0 = head listaOrdenada
           listaPorAngulo = p0 : (sortBy (ordenAngulo p0) (tail listaOrdenada))

dibujarPoligonoRegular :: Lienzo -> Posicion -> Int -> Int -> Char -> Lienzo
dibujarPoligonoRegular lienzo posInicial numLados lado c =
    (dibujarContorno lienzo (obtenerPuntosPoligono posInicial numLados lado) c)

-- Funciones para dibujar "strips" de triangulos
dibujarStrip :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarStrip lienzo lista c = triangularizar lienzo (sort lista) c

triangularizar :: Lienzo -> [Posicion] -> Char -> Lienzo
triangularizar lienzo (pos1:pos2:pos3:posiciones) c =
    triangularizar 
        (lineaEntreDosPuntos 
         (lineaEntreDosPuntos 
          (lineaEntreDosPuntos lienzo pos1 pos2 c) 
         pos1 pos3 c)
        pos2 pos3 c)
        (pos2:pos3:posiciones) c
triangularizar lienzo _ _ = lienzo
