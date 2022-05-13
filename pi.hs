-- input determines the start of denominator
-- returns tuple (value, end of denominator)
fract :: (Fractional a) => a -> (a, a)
fract n = (4.0 / (n * (n+1) * (n+2)), n+2)

fractPair :: (Fractional a) => a -> (a, a)
fractPair n = let p1 = fract n
                  p2 = fract $ snd p1
              in ((fst p1) - (fst p2), snd p2)
--fractPair n = ((fst $ fract n)-(fst $ fract $ snd $ fract n), snd $ fract $ snd $ fract n)

-- input determines amount of recursions -> accuraccy
calcPi :: (Fractional a, Eq a) => a -> a
calcPi n = 3 + (calcPi' n 0 0 2) where
  calcPi' n i accVal accN
    | n == i = accVal
    | otherwise = let p = fractPair accN in calcPi' n (i+1) (accVal + fst p) (snd p)
