main :: IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462

isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && (notElem True [0 == mod x c | c <- [2..x-1]])

conatins :: Int -> Int -> Bool
conatins x d
 | x == 0 = False
 | mod x 10 == d = True
 | otherwise = conatins (div x 10) d

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes x d = foldl1 (+) $ helper x d 1 []
 where
    helper :: Int -> Int -> Int -> [Int] -> [Int]
    helper x d num xs
     | length xs == x = xs
     | isPrimeLC num && conatins num d = helper x d (num + 1) (xs ++ [num])
     | otherwise = helper x d (num + 1) xs     