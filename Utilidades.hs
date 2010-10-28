module Utilidades where
import Data.List
import Debug.Trace

reemplazar :: a -> Int -> [a] -> [a]
reemplazar c i (x:xs)
           | i == 0 = (c:xs)
           | otherwise = x:(reemplazar c (i-1) xs)

--reemplazar c i l = reemplazar' c i l []
--    where reemplazar' c i (x:xs) acum
--           | i == 0 = reverse acum ++ (c:xs)
--           | otherwise = reemplazar' c (i-1) xs (x:acum)
--
--reemplazar e pos l = second $ foldl' f (0, [], l) l 
--                    where f (i, lista, t) x
--                             | i < 0 = (i, lista, t)
--                             | i == pos = (-1, foldl' (flip (:)) (e:tail t) lista, [])
--                             | otherwise = (i + 1, x:lista, tail t)
--                          second (_, x, _) = x

aRadianes :: Float -> Float
aRadianes = (* (pi/180))

aGrados :: Float -> Float
aGrados = (* ((180)/pi))

ordenTuplas :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
ordenTuplas (x, y) (x', y') = case y `compare` y' of
                               EQ -> x `compare` x'
                               c -> c


