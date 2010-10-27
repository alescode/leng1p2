module Utilidades where

reemplazar :: a -> Int -> [a] -> [a]
reemplazar e pos l = second $ foldl f (0, [], l) l 
                    where f (i, lista, t) x
                             | i < 0 = (i, lista, t)
                             | i == pos = (-1, foldl (flip (:)) (e:tail t) lista, [])
                             | otherwise = (i + 1, x:lista, tail t)
                          second (_, x, _) = x


--where sustituirCaracter (w:ws) y1 c
--            | y1 == 0 = (c:ws)
--            | otherwise = w:(sustituirCaracter ws (y1-1) c)

