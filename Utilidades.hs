module Utilidades where
import Data.List

reemplazar :: a -> Int -> [a] -> [a]
reemplazar c i (x:xs)
           | i == 0 = (c:xs)
           | otherwise = x:(reemplazar c (i-1) xs)

aRadianes :: Float -> Float
aRadianes = (* (pi/180))

aGrados :: Float -> Float
aGrados = (* ((180)/pi))

ordenTuplas :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
ordenTuplas (x, y) (x', y') = case y `compare` y' of
                               EQ -> x `compare` x'
                               c -> c

ordenAngulo :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Ordering
ordenAngulo (x0, y0) p1@(x1, y1) p2@(x2, y2) = angulo p1 `compare` angulo p2
           where angulo (x, y) = atan $ (fromIntegral (x0-x))/(fromIntegral (y-y0))

