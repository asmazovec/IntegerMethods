module Methods7 where

import Data.List
import Methods2

lagrange  
    :: [(Double, Double)]  -- cords function
    -> Vector              -- solution
lagrange c = gauss m
  where
    xs = map fst $ sortOn fst c
    ys = map snd $ sortOn fst c
    m = zipWith (++) (map (f) xs) (transpose [ys])
    f x = take (length xs) $ iterate (*x) 1

solveLagrange 
    :: [(Double, Double)] -- cords function
    -> Double -- x
    -> Double -- f(x) -- solution
solveLagrange c x = sum $ zipWith (*) (iterate (*x) 1) (lagrange c)


type Solution = Double
type Epsilon  = Double


-- squares left
squaresl 
    :: [(Double, Double)] -- cords function 
    -> Double   -- left edge
    -> Double   -- right edge
    -> Int      -- steps
    -> Solution -- integral
squaresl c l r s = (dx *) . sum $ map (solveLagrange c) interval
  where
    dx = (r - l) / (fromIntegral s)
    interval = [l + dx * fromIntegral x | x <- [0 .. s-1]]
  
-- squares right
squaresr
    :: [(Double, Double)] -- cords function 
    -> Double   -- left edge
    -> Double   -- right edge
    -> Int      -- steps
    -> Solution -- integral
squaresr c l r s = (dx *) . sum $ map (solveLagrange c) interval
  where
    dx = (r - l) / (fromIntegral s)
    interval = [l + dx * fromIntegral x | x <- [1 .. s]]

-- trapeze
trapeze
    :: [(Double, Double)] -- cords function 
    -> Double   -- left edge
    -> Double   -- right edge
    -> Int      -- steps
    -> Solution -- integral
trapeze c l r s = (dx * 0.5 *) . sum $ zipWith (+) <*> tail $ map (solveLagrange c) interval 
  where
    dx = (r - l) / (fromIntegral s)
    interval = [l + dx * fromIntegral x | x <- [0 .. s]]

-- Simpson
simpson 
    :: [(Double, Double)] -- cords function 
    -> Double   -- left edge
    -> Double   -- right edge
    -> Int      -- steps
    -> Solution -- integral
simpson c l r s = ((dx/6) *) . sum $ zipWith ((+) . (*4)) (map (solveLagrange c) middles) $ zipWith (+) <*> tail $ map (solveLagrange c) interval 
  where
    dx = (r - l) / (fromIntegral s)
    middles = zipWith ((.) (/2) . (+)) <*> tail $ interval
    interval = [l + dx * fromIntegral x | x <- [0 .. s]]

