import Debug.Trace

data Lienzo = MkLienzo (Int, Int) [[Char]]
type Posicion = (Int, Int)

instance (Show Lienzo) where
    show (MkLienzo (x, y) z) = foldl (++) [] [take (y+1) $ repeat '*', "*\n*", concatMap (++ "*\n*") z, (take (y+1) $ repeat '*')]

lienzoValido :: Lienzo -> Bool
lienzoValido (MkLienzo (0, m) []) = True -- ??
lienzoValido (MkLienzo (n + 1, m) lista) = (n + 1 == length lista) && 
                                           (and $ map (\xs -> m == length xs) lista)
lienzoValido _ = False 

lienzoVacio :: (Int, Int) -> Lienzo
lienzoVacio (x, y)
    | x < 0 || y < 0 = error "Las dimensiones del lienzo deben ser no negativas"
    | otherwise = MkLienzo (x, y) (take x $ repeat $ take y $ repeat ' ') 
                              
                                    
--Asumiendo que las coordenadas comienzan en 0 0
dibujarPunto :: Lienzo -> Posicion -> Char -> Lienzo
dibujarPunto (MkLienzo (x, y) lista) (x1, y1) c
    | x1 >= x || y1 >= y = error "El punto esta fuera del lienzo"
    | otherwise = MkLienzo (x, y) (sustituirLista lista x1 y1 c)
        where sustituirLista (z:zs) x1 y1 c
                    | x1 == 0 = (sustituirCaracter z y1 c):zs
                    | otherwise = z:(sustituirLista zs (x1-1) y1 c) 
                        where sustituirCaracter (w:ws) y1 c
                                    | y1 == 0 = (c:ws)
                                    | otherwise = w:(sustituirCaracter ws (y1-1) c)
                                    
obtenerColor :: Lienzo -> Posicion -> Char
obtenerColor (MkLienzo (x, y) lista) (x1, y1)
    | x1 >= x || y1 >= y = error "El punto esta fuera del lienzo"
    | otherwise = (lista !! x1) !! y1 
    
-- Punto inicial -> Angulo -> Longitud -> Puntos de la linea
dibujarLinea :: Lienzo -> Posicion -> Float -> Int -> Char -> Lienzo
dibujarLinea lienzo pos ang l c = dibujarPuntos lienzo (obtenerLinea pos ang l) c

-- Punto inicial -> Angulo -> Longitud -> Puntos de la linea
obtenerLinea :: Posicion -> Float -> Int -> [Posicion]
obtenerLinea (x,y) ang falta
    | falta > 0 = posicionNueva : obtenerLinea (x,y) ang (falta-1)
    | falta == 0 = []
    | falta < 0 = error "La longitud es negativa"
        where falta' = fromIntegral falta
              posicionNueva = (truncate $ fromIntegral x - falta' * sin ang, 
                               truncate $ fromIntegral y + falta' * cos ang)

dibujarPuntos :: Lienzo -> [Posicion] -> Char -> Lienzo
dibujarPuntos lienzo@(MkLienzo (x, y) lista) ((x1, y1):xs) c
    | x <= x1 || y <= y1 = dibujarPuntos lienzo xs c
    | otherwise = dibujarPuntos (dibujarPunto lienzo (x1, y1) c) xs c
dibujarPuntos lienzo [] _ = lienzo

--dibujarCirculo :: Lienzo -> Posicion -> Int -> Char -> Lienzo
--dibujarCirculo lienzo pos r c = dibujarPuntos (obtenerCircunferencia pos r 360)
--
--obtenerCirc :: Posicion -> Int -> Int -> [Posicion]
--obtenerCirc (x,y) r ang = ()
--x(t) = -1 + r*cos(t)
--y(y) = -1 + r*sen(t)  
--
