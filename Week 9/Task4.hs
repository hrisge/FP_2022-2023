main :: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]

isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && (notElem True [0 == mod x c | c <- [2..x-1]])

primesInRange :: Int -> Int -> [Int]
primesInRange x y = filter isPrimeLC [min x y..max y x]