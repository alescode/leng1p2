module Lienzo (Lienzo,
               Posicion,
               dimensiones,
               lienzoValido,
               lienzoVacio,
               lienzoNuevo,
               dibujarPunto,
               obtenerColor,
               dibujarLinea,
               dibujarCirculo,
               llenar) where

import Utilidades

data Lienzo = MkLienzo { dimensiones :: (Int, Int),
                         matriz :: [[Char]] }

type Posicion = (Int, Int)

instance (Show Lienzo) where
    show (MkLienzo (x, y) z) = foldl (++) []
     [take (y+1) $ repeat '*', "*\n*", concatMap (++ "*\n*") z, (take (y+1) $ repeat '*')]

lienzoValido :: Lienzo -> Bool
lienzoValido (MkLienzo (0, m) []) = True -- ??
lienzoValido (MkLienzo (n + 1, m) lista) = (n + 1 == length lista) && 
                                           (and $ map (\xs -> m == length xs) lista)
lienzoValido _ = False 

lienzoNuevo :: (Int, Int) -> Char -> Lienzo
lienzoNuevo (x, y) c
    | x < 0 || y < 0 = error "Las dimensiones del lienzo deben ser no negativas"
    | otherwise = MkLienzo (x, y) (take x $ repeat $ take y $ repeat c) -- repeat.take

lienzoVacio :: (Int, Int) -> Lienzo
lienzoVacio pos = lienzoNuevo pos ' ' 
                              
obtenerColor :: Lienzo -> Posicion -> Char
obtenerColor (MkLienzo (x, y) lista) (x1, y1)
    | x1 >= x || y1 >= y = error "El punto esta fuera del lienzo"
    | otherwise = lista !! x1 !! y1 
    
-- Más compacto? otro reemplazar
dibujarPunto :: Lienzo -> Posicion -> Char -> Lienzo
dibujarPunto (MkLienzo (x, y) lista) (x1, y1) c
    | x1 >= x || y1 >= y = error "El punto esta fuera del lienzo"
    | otherwise = MkLienzo (x, y) (sustituirLista lista x1 y1 c)
        where sustituirLista (z:zs) x1 y1 c
                    | x1 == 0 = (reemplazar c y1 z):zs
                    | otherwise = z:(sustituirLista zs (x1-1) y1 c) 

-- Punto inicial -> Angulo -> Longitud -> Puntos de la linea
obtenerLinea :: Posicion -> Float -> Int -> [Posicion]
obtenerLinea (x,y) ang l
    | l > 0 =  posicionNueva : obtenerLinea (x,y) ang (l-1)
    | l == 0 = [posicionNueva]
    | l < 0 = error "La longitud es negativa"
        where posicionNueva = (round $ fromIntegral x - l' * sin ang, 
                               round $ fromIntegral y + l' * cos ang)
              l' = fromIntegral l

obtenerCirculo :: Posicion -> Int -> Int -> [Posicion]
obtenerCirculo (x,y) r l
    | l > 0 = posicionNueva : obtenerCirculo (x,y) r (l-1)
    | l == 0 = [posicionNueva]
    | l < 0 = error "La longitud es negativa"
        where l' = fromIntegral l
              r' = fromIntegral r
              posicionNueva = (round $ fromIntegral x + r' * sin l', 
                               round $ fromIntegral y + r' * cos l')

-- como hacer esto con map y filter?
dibujarPuntos :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarPuntos lienzo@(MkLienzo (x, y) lista) ((x1, y1):xs) c
    | fueraDelLienzo = dibujarPuntos lienzo xs c
    | otherwise = dibujarPuntos (dibujarPunto lienzo (x1, y1) c) xs c
        where fueraDelLienzo = x <= x1 || y <= y1 || x1 < 0 || y1 < 0
dibujarPuntos lienzo [] _ = lienzo

-- Punto inicial -> Angulo -> Longitud -> Puntos de la linea
dibujarLinea :: Lienzo -> Posicion -> Float -> Int -> Char -> Lienzo
dibujarLinea lienzo pos ang l c = dibujarPuntos lienzo (obtenerLinea pos ang l) c

dibujarCirculo :: Lienzo -> Posicion -> Int -> Char -> Lienzo
dibujarCirculo lienzo pos r c = dibujarPuntos lienzo (obtenerCirculo pos r 360) c

llenar :: Lienzo -> Posicion -> Char -> Lienzo
llenar lienzo@(MkLienzo (x, y) lista) pos@(x1, y1) c
        | fueraDelLienzo = error "La posicion se encuentra fuera del lienzo"
        | otherwise = rellenar lienzo (obtenerColor lienzo pos) c pos
            where fueraDelLienzo = x <= x1 || y <= y1 || x1 < 0 || y1 < 0 

rellenar :: Lienzo -> Char -> Char -> Posicion -> Lienzo
rellenar lienzo@(MkLienzo (x, y) lista) c1 c2 pos@(x1, y1)
    | fueraDelLienzo || obtenerColor lienzo pos /= c1 = lienzo
    | otherwise = rellenar (
                    rellenar (
                      rellenar (
                        rellenar (dibujarPunto lienzo pos c2) c1 c2 (x1 + 1, y1)
                      ) c1 c2 (x1 - 1, y1)
                    ) c1 c2 (x1, y1 + 1)
                  ) c1 c2 (x1, y1 - 1)
        where fueraDelLienzo = x <= x1 || y <= y1 || x1 < 0 || y1 < 0    
