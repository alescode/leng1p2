module Instrucciones where

import Lienzo

type Magnitud = Int

data Instruccion = Limpiar
                | EstablecerColor Char 
                | ObtenerColor Posicion 
                | DibujarPunto Posicion 
                | DibujarLinea Posicion Posicion
                | Llenar Posicion 
                | DibujarCirculo Posicion Magnitud 
                | DibujarCurva Posicion Magnitud Magnitud 
                | DibujarPoligono [Posicion] Bool 
                | DibujarPoligonoRegular Posicion Int Int
                | Triangularizar [Posicion]

interpretarComando :: (Lienzo, Char) -> Instruccion -> (Lienzo, Char)
interpretarComando (lienzo, c) Limpiar = 
    (lienzoNuevo (dimensiones lienzo) c, c)
interpretarComando (lienzo, c) (EstablecerColor c1) = (lienzo,  c1)
interpretarComando (lienzo, c) (ObtenerColor pos) = (lienzo, obtenerColor lienzo pos)
interpretarComando (lienzo, c) (DibujarPunto pos) = (dibujarPunto lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarLinea p1 p2) = 
    (lineaEntreDosPuntos lienzo p1 p2 c, c)
interpretarComando (lienzo, c) (Llenar pos) = 
    (llenar lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarCirculo pos r) =
    (dibujarCirculo lienzo pos r c, c)
interpretarComando (lienzo, c) (DibujarCurva pos r l) =
    (dibujarCurva lienzo pos r l c, c)
interpretarComando (lienzo, c) (DibujarPoligono pos co) =
    (dibujarPoligono co lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarPoligonoRegular pos lados apotema) =
    (dibujarPoligonoRegular lienzo pos lados apotema c, c)

interpretarBatch :: (Int, Int) -> [Instruccion] -> Lienzo
interpretarBatch dim [] = lienzoVacio dim
--interpretarBatch dim [i] = interpretarComando (lienzoVacio dim)

--dibujarCirculo (dibujarCirculo (llenar (dibujarCirculo (llenar (lienzoVacio (30,100)) (15,50) 'c') (15,50) 14 ' ') (15,50) ' ') (3, 33) 6 ' ') (3, 67) 6 ' '
