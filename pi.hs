import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Decimal
import System.IO


-- input determines the start of denominator
-- returns tuple (value, end of denominator)
fract :: Decimal -> (Decimal, Decimal)
fract n = (4.0 / (n * (n+1) * (n+2)), n+2)


fractPair :: Decimal -> (Decimal, Decimal)
fractPair n = let p1 = fract n
                  p2 = fract $ snd p1
              in ((fst p1) - (fst p2), snd p2)


-- input determines amount of recursions -> accuraccy
calcPi :: Decimal -> Decimal
calcPi n = 3 + (calcPi' n 0 0 2) where
  calcPi' n i accVal accN
    | n == i = accVal
    | otherwise = let p = fractPair accN in calcPi' n (i+1) (accVal + fst p) (snd p)


milli :: IO Integer
milli = (round . (* 1000)) <$> getPOSIXTime


main' :: Integer -> Integer -> IO ()
main' n i = do
  fileHandle <- openFile "pi.txt" AppendMode
  start <- milli
  let pi' = calcPi (fromIntegral i)
  let str1 = (show i) ++ ": " ++ (show pi') ++ " in "
  putStr str1
  hPutStr fileHandle str1
  end <- milli
  let str2 = show (end - start) ++ "ms"
  putStrLn str2
  hPutStrLn fileHandle str2
  hClose fileHandle
  if n <= i
    then return ()
    else main' n (i+1)


main :: IO ()
main = do
  putStrLn "minimum recursions:"
  minRecString <- getLine
  let minRec = read minRecString :: Integer
  putStrLn "amount of additional recursions:"
  addRecString <- getLine
  let addRec = read addRecString :: Integer
  main' (minRec + addRec) minRec
