module MMatrix where

import Data.List

type Matrix = [[ Float ]]
type Vector = [ Float ]


echalon :: Matrix -> Matrix
echalon m = do
    let divFst m' = do
        x <- m'
        return $ 1 : map (/head x) (tail x)

    let minus m' v = do
        x <- m'
        return $ zipWith (-) (tail x) (tail v)

    let mMinus = minus <$> tail <*> head $ divFst m

    if length m > 0
       then (head $ divFst m) : map (0:) (echalon mMinus)
       else []

    
gauss :: Matrix -> Vector
gauss m = do
    let remEchalon m' = do
        if length m' > 0
           then (tail $ head m') : (remEchalon $ map tail $ tail m')
           else []
    
    let eval xi m' = do
        xs <- m'
        xm <- ((*xi) $ head xs)-(xs!!1) : []

        if length m' > 0
           then (head $ head m') : (eval (head $ head m') (tail m'))
           else []
        
    let mm = reverse . remEchalon . echalon $ m

    if length mm > 0
       then eval (head $ head mm) (tail mm)
       else []
    
    {-
    if length mm > 0
       then (head $ head mm) : (eval (head $ head mm) (tail mm))
       else []
       -}


show' :: Matrix -> String
show' = ('[':) . (++"]") . intercalate "\n" . map (unwords . map show)
