module Methods 
    ( dichotomy
    , chords
    , newton
    ) where

import MathParser
import Data.List ( iterate' )


dichotomy :: 
     Expr      -- ^ Expression
  -> Double    -- ^ Left edge a
  -> Double    -- ^ Right edge b
  -> Double    -- ^ Epsilon
  -> (Double, Double)    -- ^ (Result, Result Epsi)
dichotomy expr a b epsi = (fst res,  abs $ (fst $ snd res) - (snd $ snd res))
  where
    res = head $ dropWhile fine iters

    fine (m, (l, r)) = or $
        [ (abs $ (r - l) / 2)  >= epsi -- error by X
        , (abs $ eval' expr m) >= epsi -- error by Y
        ]

    next (m, (l, r)) 
        | eval' expr l * eval' expr m <= 0 = (xi l m, (l, m))
        | otherwise                        = (xi m r, (m, r))
    
    iters = iterate' next (xi a b, (a, b))

    xi l r = (l + r) / 2 -- i-тое приближение


chords :: 
     Expr      -- ^ Expression
  -> Double    -- ^ Left edge a
  -> Double    -- ^ Right edge b
  -> Double    -- ^ Epsilon
  -> (Double, Double)    -- ^ Result
chords expr a b epsi = (fst $ fst res, abs $ (fst $ fst res) - (fst $ snd res))
  where
    res = head $ dropWhile fine pairs

    fine ((m0, _), (m1, _)) = or $
        [ (abs $ (m1 - m0) / 2)                 >= epsi -- error by X
        , (abs $ eval' expr m1 - eval' expr m0) >= epsi -- error by Y
        ]

    next (m1, (l1, r1)) 
        | eval' expr l1 * eval' expr m1 <= 0 = (xi l1 m1, (l1, m1))
        | otherwise                          = (xi m1 r1, (m1, r1))

    iters = iterate' next (xi a b, (a, b))

    pairs = zip iters (tail iters)

    xi l r = l-(eval' expr l)/((eval' expr r)-(eval' expr l))*(r-l)


newton :: 
     Expr      -- ^ Expression
  -> Double    -- ^ Left edge a
  -> Double    -- ^ Right edge b
  -> Double    -- ^ Epsilon
  -> (Double, Double)    -- ^ Result
newton expr a b epsi = (fst res, abs $ (fst res) - (snd res))
  where
    res = head $ dropWhile fine pairs

    fine (m0, m1) = or $
        [ (abs $ (m1 - m0) / 2)                 > epsi -- error by X
        , (abs $ eval' expr m1 - eval' expr m0) > epsi -- error by Y
        ]
     
    next m0 = m0-(eval' expr m0)/(deriv1 m0)
    
    iters = iterate' next x0

    x0
        | eval' expr a * deriv2 a > 0 = a
        | otherwise                   = b

    pairs = zip iters (tail iters)

    deriv1 x = (eval' expr (x+epsi)-eval' expr (x-epsi))/(2*epsi)
    deriv2 x = (deriv1 (x+epsi)-deriv1 (x-epsi))/(4*epsi*epsi)

