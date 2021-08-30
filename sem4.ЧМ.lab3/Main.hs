import Methods 
import qualified M 
import Text.Printf
import Data.List hiding ( break )

main :: IO ()
main = do
    let m   = [[1.6, 2.3, 1.2],[2.3, 0.6, 1.5],[1.2,1.5,3.8]]
    let eps = 1E-5
    putStrLn $ showMatrixIters "%7.3F" $ convsA m
    putStrLn "\nMatrix S"
    putStrLn $ showMatrix "%7.3F" $ matrixS m
    putStrLn "\nFrobenius matrix P"
    putStrLn $ showMatrix "%7.3F" $ frobenius m
    putStrLn ""

    putStrLn $ do 
        (lamda, vector, koef, i) <- zipWith4 (\a b c d -> (a, b, c, d)) (self m eps) (selfVec m eps) (reductions m eps) (iterate (+1) (1 :: Int))
        printf  "λ%d          = %7.3F" i lamda ++ printf "\n" ++
         printf "|A - λ%dE|   = %7.3E" i (M.det (subMatrix m (mulScal matrixE lamda))) ++ printf "\n" ++
         printf "x%d          = " i ++ showMatrix "%7.3F" [vector] ++ printf "\n" ++
         printf "Ax%d - λ%dx%d  = " i i i ++ showMatrix "%7.3E" [subVector (mulVector m vector) (map (*lamda) vector)] ++ printf "\n" ++
         printf "k%d          = %d" i koef ++ printf "\n" ++
         printf "\n"

    return ()


showMatrix :: String -> Matrix -> String
showMatrix _ []     = "[]"
showMatrix _ [[]]   = "[]"
showMatrix f (l:[]) = ("[ "++) . (++" ]") . intercalate ", " $ map (printf f) l
showMatrix f m      = showPrior ++ showTail ++ showBreak
  where
    showPrior = ("⎡ "++) . (++" ⎤\n") . intercalate ", " . map (printf f) $ head m
    showTail  = concat . map (("⎢ "++) . (++" ⎥\n") . intercalate ", " . map (printf f)) . init $ tail m
    showBreak = ("⎣ "++) . (++" ⎦") . intercalate ", " . map (printf f) $ last m


showMatrixIters :: String -> [Matrix] -> String
showMatrixIters f ms = intercalate "\n\n" $ map (showMatrix f) ms



