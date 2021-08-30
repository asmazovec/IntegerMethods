module Methods
    ( Matrix
    , Vector
    , matrixE
    , echelonsTriangle
    , echelons
    , echelonTriangle 
    , echelon
    , gausses
    , gauss
    , det
    , equationsE 
    , inverse
    ) where

import Data.List

type Matrix = [ Vector ]
type Vector = [ Double ]


--- Echelon --- --- --- --- --- --- --- --- --- --- --- --- --- ---

echelon :: Matrix -> Matrix
echelon = last . echelons

echelons :: Matrix -> [Matrix]
echelons = zipWith (zipWith (++)) zeros . echelonsTriangle
  where
    zeros  = (++) <*> take 4. repeat . last <$> zeros'
    zeros' = scanl (\acc x -> acc ++ [x]) [[]] (tail . inits $ repeat 0)


echelonTriangle :: Matrix -> Matrix
echelonTriangle = last . echelonsTriangle

echelonsTriangle :: Matrix -> [Matrix]
echelonsTriangle = (zipWith (++) <$> rows <*> id) . echelons'
  where
    rows = scanl (\acc x -> acc ++ [(1:) . divFst $ head x]) []


echelons' :: Matrix -> [Matrix]
echelons' = (++[[]]) . takeWhile (/=[]) . iterate (subFst . map divFst)

subFst :: Matrix -> Matrix
subFst m = do
    vi <- tail m
    return $ zipWith (-) vi (head m)

divFst :: Vector -> Vector
divFst v = map (/ head v) (tail v)


--- Gauss --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

gauss :: Matrix -> Vector
gauss = reverse . concat . map head . gausses 

gausses :: Matrix -> [Matrix]
gausses
  = takeWhile (/= []) 
  . iterate step 
  . reverse 
  . map (reverse . tail) 
  . echelonTriangle 
  where
    step m' = do
        vi <- tail m'
        let vTail = tail . tail $ vi
        let vVarI = head . tail $ vi
        let vResI = head $ vi
        let vVarF = head . head $ m'

        return $ vResI - vVarI * vVarF : vTail


--- Determinant -- --- --- --- --- --- --- --- --- --- --- --- --- ---

det :: Matrix -> Double
det = product . concat . map (map head) . echelons'


--- Inverse Matrix --- --- --- --- --- --- --- --- --- --- --- --- --- 

matrixE :: Matrix
matrixE = do 
    vi <- inits $ repeat 0
    return $ vi ++ (1: repeat 0)

equationsE :: Matrix -> [Matrix]
equationsE mm = zipWith (\m e -> zipWith (\mi ei -> mi ++ [ei]) m e) (replicate (length mm) mm) matrixE

inverse :: Matrix -> Matrix
inverse = transpose . map gauss . equationsE





