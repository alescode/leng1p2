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
--                 | DibujarPoligono [Posicion] Bool 
--                 | DibujarPoligonoRegular Posicion Integer Integer 
--                 | Triangularizar [Posicion]

interpretarComando :: (Lienzo, Char) -> Instruccion -> (Lienzo, Char)
interpretarComando (lienzo, c) Limpiar = (lienzoNuevo (dimensiones lienzo) c, c)
interpretarComando (lienzo, c) (EstablecerColor c1) = (lienzo,  c1)
interpretarComando (lienzo, c) (ObtenerColor pos) = (lienzo, obtenerColor lienzo pos)
interpretarComando (lienzo, c) (DibujarPunto pos) = (dibujarPunto lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarLinea (x1, y1) (x2, y2)) = (dibujarLinea lienzo (x1, y1) alfa hipotenusa c, c)
    where alfa = atan $ (fromIntegral (y1 - y2))/(fromIntegral (x1-x2))
          hipotenusa = truncate $ sqrt $ fromIntegral ((x1 - x2)^2 + (y1 - y2)^2)
interpretarComando (lienzo, c) (Llenar pos) = (llenar lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarCirculo pos r) = (dibujarCirculo lienzo pos r c, c)
interpretarComando (lienzo, c) (DibujarCurva pos r l) = (dibujarCurva lienzo pos r l c, c)

--dibujarCirculo (dibujarCirculo (llenar (dibujarCirculo (llenar (lienzoVacio (30,100)) (15,50) 'c') (15,50) 14 ' ') (15,50) ' ') (3, 33) 6 ' ') (3, 67) 6 ' '
