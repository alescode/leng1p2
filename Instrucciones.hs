module Instrucciones where

import Lienzo
import Debug.Trace

type Magnitud = Int

data Instruccion = Limpiar
                | EstablecerColor Char 
                | ObtenerColor Posicion 
                | DibujarPunto Posicion 
                | DibujarLinea Posicion Posicion
                | Llenado Posicion 
                | DibujarCirculo Posicion Magnitud 
                | DibujarCurva Posicion Magnitud Magnitud 
                | DibujarPoligono [Posicion] Bool 
                | DibujarPoligonoRegular Posicion Int Int
                | Triangularizar [Posicion]
                | Nada
                deriving (Show)

leerDimensiones :: String -> (Int, Int)
leerDimensiones = leerD . words
    where leerD ("dim":x:y:[]) = (read x, read y)
          leerD _ = error "Error de formato en la cabecera"

leerInstruccion :: String -> Instruccion
leerInstruccion = leerI . words
    where
      leerI ("color":x:[]) = EstablecerColor $ head x
      leerI ("get":x:y:[]) = ObtenerColor (read x, read y)
      leerI ("point":x:y:[]) = DibujarPunto (read x, read y)
      leerI ("line":x1:y1:x2:y2:[]) = 
          DibujarLinea (read x1, read y1) (read x2, read y2)
      leerI ("circle":x:y:r:[]) = 
          DibujarCirculo (read x, read y) (read r)
      leerI ("fill":x:y:[]) = 
          Llenado (read x, read y)
      leerI ("curve":x:y:r:a:[]) = 
          DibujarCurva (read x, read y) (read r) (read a) 
      leerI ("cpolygon":n:puntos) 
          | (length puntos) /= 2*(read n) = error "Error de formato en cpolygon"
          | otherwise = DibujarPoligono (leerPuntos puntos []) True
      leerI ("polygon":n:puntos) 
          | (length puntos) /= 2*(read n) = error "Error de formato en polygon"
          | otherwise = DibujarPoligono (leerPuntos puntos []) False
      leerI ("rpolygon":x:y:n:l:[]) = 
          DibujarPoligonoRegular (read x, read y) (read n) (read l)
      leerI ("triangularize":n:puntos) 
          | (length puntos) /= 2*(read n) = error "Error de formato en triangularize"
          | otherwise = Triangularizar (leerPuntos puntos [])
      leerI _ = Nada

leerPuntos :: [String] -> [(Int, Int)] -> [(Int, Int)]
leerPuntos (x:y:puntos) acum = leerPuntos puntos ((read x, read y):acum)
leerPuntos [] acum = reverse acum

interpretarComando :: (Lienzo, Char) -> Instruccion -> (Lienzo, Char)
interpretarComando (lienzo, c) Limpiar = 
    (lienzoNuevo (dimensiones lienzo) c, c)
interpretarComando (lienzo, c) (EstablecerColor c1) = (lienzo,  c1)
interpretarComando (lienzo, c) (ObtenerColor pos) = (lienzo, obtenerColor lienzo pos)
interpretarComando (lienzo, c) (DibujarPunto pos) = (dibujarPunto lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarLinea p1 p2) = 
    (lineaEntreDosPuntos lienzo p1 p2 c, c)
interpretarComando (lienzo, c) (Llenado pos) = 
    (llenar lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarCirculo pos r) =
    (dibujarCirculo lienzo pos r c, c)
interpretarComando (lienzo, c) (DibujarCurva pos r l) =
    (dibujarCurva lienzo pos r l c, c)
interpretarComando (lienzo, c) (DibujarPoligono pos co) =
    (dibujarPoligono co lienzo pos c, c)
interpretarComando (lienzo, c) (DibujarPoligonoRegular pos numLados lado) =
    (dibujarPoligonoRegular lienzo pos numLados lado c, c)
interpretarComando (lienzo, c) Nada = (lienzo, c)

interpretarBatch :: (Int, Int) -> [Instruccion] -> Lienzo
interpretarBatch dim [] = lienzoVacio dim
interpretarBatch dim lista = fst $ foldl realizarInstruccion (lienzoVacio dim, ' ') lista
    where realizarInstruccion (lienzo, c) instr = interpretarComando (lienzo, c) instr 
--interpretarBatch dim [i] = interpretarComando (lienzoVacio dim) 

--dibujarCirculo (dibujarCirculo (llenar (dibujarCirculo (llenar (lienzoVacio (30,100)) (15,50) 'c') (15,50) 14 ' ') (15,50) ' ') (3, 33) 6 ' ') (3, 67) 6 ' '
