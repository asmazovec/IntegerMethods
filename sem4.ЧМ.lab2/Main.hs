import Methods
import System.IO
import Data.List
import Text.Printf

main :: IO ()
main = do
    let inp = "I"

    hInp    <- openFile inp ReadMode
    matrixX <- map (map read . words) . lines <$> hGetContents hInp :: IO Matrix
    matrix  <- return $ map init matrixX
    xs      <- return $ map last matrixX

    putStrLn "\n━┫ Echelon matrix (iterations) ┣━━━━━"
    putStrLn . showMatrixIters "%7.3F" $ echelons matrixX

    putStrLn "\n━┫ Inverse matrix A (equations) ┣━━━━"
    putStrLn . showMatrixIters "%7.3F" $ equationsE matrix

    putStrLn "\n━┫ Gauss solution (iterations) ┣━━━━━" 
    putStrLn . showMatrixIters "%7.3F" $ gausses matrixX

    putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
    putStrLn ""

    putStrLn "\n━ Gauss solution (result)" 
    putStrLn . showVector "%7.3F" $ gauss matrixX
    
    putStrLn "\n━ Vector epsilon"
    let vEpsi = zipWith (-) (map sum $ map (zipWith (*) (gauss matrixX)) matrix) xs
    putStrLn $ showVector "% -12.5E" vEpsi

    putStrLn "\n━ Normal vector epsilon"
    putStrLn . printf "%-12.5E" $ sqrt . sum $ map (** 2) vEpsi

    putStrLn "\n-------------------------------------"

    putStrLn "\n━ Det matrix A (result)"
    putStrLn . printf "%7.3F" $ det matrix

    putStrLn "\n━ Inverse matrix A (result)"
    putStrLn . showMatrix "%7.3F" $ inverse matrix

    putStrLn "\n━ Matrix epsilon (AX - E)"
    let mEpsi = subMatrix (mulMatrix matrix (inverse matrix)) matrixE 
    putStrLn $ showMatrix "% -12.5E" mEpsi

    putStrLn "\n━ Normal matrix epsilon"
    putStrLn . printf "%-12.5E" . sqrt . sum . map (** 2) $ concat mEpsi

    return ()


mulMatrix :: Matrix -> Matrix -> Matrix
mulMatrix a b = do
    a' <- a
    b' <- return $ map (sum . zipWith (*) a') $ transpose b
    return b'

subMatrix :: Matrix -> Matrix -> Matrix
subMatrix a b = zipWith (zipWith (-)) a b



showMatrix :: String -> Matrix -> String
showMatrix f m = ('[':) . (++" ]") . intercalate "\n " $ map (intercalate ", " . map (printf f)) m

showMatrixIters :: String -> [Matrix] -> String
showMatrixIters f ms = intercalate "\n\n" $ map (showMatrix f) ms


showVector :: String -> Vector -> String
showVector f v = ('(':) . (++" )") . intercalate ", " $ map (printf f) v






