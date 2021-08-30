module Methods where

import Control.Applicative
import Control.Monad
import Prelude hiding ( break )
import Data.List hiding ( break )

type Matrix = [ Vector ]
type Vector = [ Double ]


matrixE :: Matrix
matrixE = do
    prior <- inits $ repeat 0
    break <- return $ repeat 0
    return $ prior ++ 1 : break

mulVector :: Matrix -> Vector -> Vector
mulVector a b = join $ mulMatrix a (transpose [b])

mulScal :: Matrix -> Double -> Matrix
mulScal a b = map (map (*b)) a

mulVector' :: Vector -> Matrix -> Vector
mulVector' a b = join $ mulMatrix (transpose [a]) b

mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix a b = do
    a' <- a
    b' <- return $ map (sum . zipWith (*) a') $ transpose b
    return b'

subMatrix :: Matrix -> Matrix -> Matrix
subMatrix = zipWith (zipWith (-))

subVector :: Vector -> Vector -> Vector
subVector = zipWith (-)


gershgorin :: Matrix -> (Double, Double)
gershgorin m = (\e -> (minimum e, maximum e)) . concat $ do
    l <- zipWith (\l i -> (l, i)) m [0..]
    let center (l', i') = l' !! i'
    let low_c  (l', i') = sum $ take i' l'
    let high_c (l', i') = sum $ drop (i' + 1) l'
    let rad    l'       = sum $ sequence [high_c, low_c] l'
    return $ sequence [(+) <$> center <*> rad, (-) <$> center <*> rad] l


self :: Matrix -> Double -> Vector
self m eps = map (\x -> (sum x) / (fromIntegral $ length x)) $
    groupBy (\a b -> abs (a - b) <= (eps * 100)) $
    filter (>= (fst $ gershgorin m) - 0.5) $ do
    let selfV = 1 : map (*(-1)) (head $ frobenius m)
    lamda <- [(fst $ gershgorin m), (fst $ gershgorin m) + eps .. (snd $ gershgorin m)]
    let root = sum $ zipWith (*) (iterate (*lamda) 1) (reverse selfV)
    return $ if (abs root) <= (eps * 100) then lamda else ((fst $ gershgorin m) - 1)


selfVec :: Matrix -> Double -> [Vector]
selfVec m eps = selfVec' $ map (\x -> take (length $ self m eps) $ iterate (*x) 1) (self m eps)
  where
    selfVec' vs = do
        v <- vs
        return $ mulVector (matrixS m) (reverse v)

reductions :: Matrix -> Double -> [Int]
reductions m eps = map red $ self m eps
  where
    red = length . filter ((<= eps * 100) . abs) . decodered
    selfV = reverse $ 1 : map (*(-1)) (head $ frobenius m)
    kd    = take (length selfV) $ zipWith (\a b -> (a, b)) (repeat 1) (iterate (+1) 0) :: [(Double, Double)]
    stepd kd' = do
        (k, d) <- kd'
        return $ if d == 0 then (k*d, 0) else (k*d, d-1)
    kds   = take (length selfV) $ iterate stepd kd
    decodekd  l = map (map (\(k, d) -> k * (l ** d))) kds
    decodered l = map (sum . zipWith (*) selfV) (decodekd l)


frobenius :: Matrix -> Matrix
frobenius a = (matrixS' a) `mulMatrix` a `mulMatrix` (matrixS a)


convsA :: Matrix -> [Matrix]
convsA = scanl step <*> liftA2 zip convsM' convsM
  where
    step a0 (m', m) = m' `mulMatrix` a0 `mulMatrix` m


-- | Matrix M{k} 
convsM  :: Matrix -> [Matrix]
convsM  a = zipWith3 (\p j b -> p ++ j : b) priorsE mkjs breaksE
  where
    -- m[i,j]
    priorsE = drop 2 . reverse $ inits matrixE'
    breaksE = tail . reverse $ tails matrixE'

    -- m[k,j]
    mkjs = zipWith3 (\p j b -> (negate . (*j) <$> p) ++ j : (negate . (*j) <$> b))
        priorsMkj mkks breaksMkj
      where
        priorsMkj = map reverse . zipWith drop [2..] $ reverse <$> ak1s
        breaksMkj = map reverse . zipWith take [1..] $ reverse <$> ak1s

    -- m[k,k]
    mkks = map ((1/) . head) . zipWith drop [1..] $ reverse <$> ak1s
    ak1s = map head . zipWith drop [0..] $ reverse <$> convsA a

    matrixE' = take (length a) <$> take (length a) matrixE


-- | Matrix M{k} inversed
convsM' :: Matrix -> [Matrix]
convsM' a = zipWith3 (\p j b -> p ++ j : b) priorsE mkjs breaksE
  where
    -- m[i,j]
    priorsE = drop 2 . reverse $ inits matrixE'
    breaksE = tail . reverse $ tails matrixE'

    -- m[k,j]
    mkjs = map head . zipWith drop [0..] $ reverse <$> convsA a

    matrixE' = take (length a) <$> take (length a) matrixE


matrixS  :: Matrix -> Matrix
matrixS  = foldl1 (\acc x -> acc `mulMatrix` x) . convsM


matrixS' :: Matrix -> Matrix
matrixS' = foldl1 (\acc x -> acc `mulMatrix` x) . reverse . convsM'




