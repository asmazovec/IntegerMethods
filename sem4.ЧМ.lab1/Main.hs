import System.IO
import Methods
import MathParser
import Text.Printf

main :: IO ()
main = do
    let inp = "input"
    let out = "output"

    hInp <- openFile inp ReadMode

    method <- (read <$> hGetLine hInp) :: IO Int
    func   <- parseExpression <$> hGetLine hInp
    [a, b] <- map (read :: String -> Double)
        .  sequence [(!!0), (!!1)] 
        .  words 
       <$> hGetLine hInp
    epsi   <- (read <$> hGetLine hInp) :: IO Double

    hClose hInp

    printf "Method    : %d\n" method
    putStr "Function  : "; print func
    printf "Interval  : (%F, %F)\n" a b
    printf "Accuracy  : %E\n" epsi
    putStrLn ""
   
    let d = dichotomy func a b epsi
    let c = chords    func a b epsi
    let n = newton    func a b epsi
    let outFormat m = m ++ " ::\n"
                  ++ "x    : %.*F\n"
                  ++ "f(x) : %.*F\n"
                  ++ "Eps  : %E\n"
                  ++ "Eps* : %.*F\n"
    let epsiN = negate $ truncate $ logBase 10 epsi :: Int

    hOut <- openFile out WriteMode

    printf (outFormat "Dichotomy") epsiN (fst d) epsiN (eval' func (fst d)) epsi epsiN (snd d)
    case method of 
        0 -> do 
            hPrintf hOut (outFormat "Dichotomy") epsiN (fst d) epsiN (eval' func (fst d)) epsi epsiN (snd d)
            return ()
        1 -> do 
            printf (outFormat "Chords") epsiN (fst c) epsiN (eval' func (fst c)) epsi epsiN (snd c)
            hPrintf hOut (outFormat "Chords") epsiN (fst c) epsiN (eval' func (fst c)) epsi epsiN (snd c)
            return ()
        2 -> do 
            printf (outFormat "Newton") epsiN (fst n) epsiN (eval' func (fst n)) epsi epsiN (snd n)
            hPrintf hOut (outFormat "Newton") epsiN (fst n) epsiN (eval' func (fst n)) epsi epsiN (snd n)
            return ()
        _ -> error "no method"

    hClose hOut

    return ()
   
   
