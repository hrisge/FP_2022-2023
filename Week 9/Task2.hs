import Data.Char

main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462


isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && (notElem True [0 == mod x c | c <- [2..x-1]])

sumSpecialPrimes ::Int -> Int -> Int
sumSpecialPrimes n d = sum (take n [c | c <- [0..] , isPrimeLC c && (elem d $ map digitToInt $ show c)])