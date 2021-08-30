import Methods7
import Text.Printf

data Grid = Even | Dynamic deriving (Show, Read)
data Func = Analitic | Table deriving (Show, Read)


dynIntegral 
    :: (Int -> Double)  -- Function
    -> Epsilon          -- epsilon
    -> (Solution, Epsilon, Int)  
dynIntegral f e = head $
    dropWhile (\(_, e', _) -> e' > e) $
    zipWith (\s ts i -> (ts, abs ((ts-s)/ts), i)) iters (tail iters) (iterate (+1) 1)
  where 
    iters = (map f (iterate (*2) 100))



main :: IO ()
main = do
    grid  <- return Even
    steps <- return 1000
    (a,b) <- return (0, 4)
    table <- return [(0,0),(2,2),(4,4)]

    printf "left squares  = %F\n" (squaresl table a b steps)
    printf "right squares = %F\n" (squaresr table a b steps)
    printf "trapeze       = %F\n" (trapeze  table a b steps)
    printf "Simpson       = %F\n" (simpson table a b steps)

    return ()

