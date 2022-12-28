main :: IO()
main = do
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9-- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45

firstElementHasDuplicates :: [Int] -> Bool
firstElementHasDuplicates xs = elem (head xs) (tail xs)

removeElement :: [Int] -> Int -> [Int]
removeElement xs x = helper xs x []
 where
    helper :: [Int] -> Int -> [Int] -> [Int]
    helper xs x res
     | null xs = res
     | head xs /= x = helper (tail xs) x (res ++ [head xs])
     | otherwise = helper (tail xs) x res


removeAllDuplicates :: [Int] -> [Int]
removeAllDuplicates xs = helper xs []
 where 
    helper :: [Int] -> [Int] -> [Int]
    helper xs res
     | null xs = res
     | firstElementHasDuplicates xs = helper (removeElement xs (head xs)) res
     | otherwise = helper (tail xs) (res ++ [head xs])

sumUnique :: [[Int]] -> Int
sumUnique xs = helper xs 0
 where
    helper :: [[Int]] -> Int -> Int
    helper xs res
     | null xs = res
     | null $ removeAllDuplicates (head xs) = helper (tail xs) res
     | otherwise = helper (tail xs) (res + foldl1 (+) (removeAllDuplicates (head xs)))   