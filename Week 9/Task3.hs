import Data.List

main :: IO()
main = do
    print $ mergeLinearRec [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
    print $ mergeLinearRec [1, 2, 3] [2] == [1, 2, 3]
    print $ mergeLinearRec [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
    print $ mergeLinearRec [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
    print $ mergeLinearRec [2] [1, 2, 3] == [1,2,3]
    print $ mergeLinearRec [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
    print $ mergeLinearRec [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]

    print $ mergeLinearXs [1, 2, 3] [2, 3, 4, 5, 6] == [1, 2, 3, 4, 5, 6]
    print $ mergeLinearXs [1, 2, 3] [2] == [1, 2, 3]
    print $ mergeLinearXs [1, 2, 3] [7, 8, 9] == [1, 2, 3, 7, 8, 9]
    print $ mergeLinearXs [2, 3, 4, 5, 6] [1, 2, 3] == [1,2,3,4,5,6]
    print $ mergeLinearXs [2] [1, 2, 3] == [1,2,3]
    print $ mergeLinearXs [7, 8, 9] [1, 2, 3] == [1,2,3,7,8,9]
    print $ mergeLinearXs [7, 9, 11] [8, 10, 12] == [7,8,9,10,11,12]

mergeLinearRec :: [Int] -> [Int] -> [Int]
mergeLinearRec xs ys = helper xs ys []
 where 
    helper :: [Int] -> [Int] -> [Int] -> [Int]
    helper xs ys res
     | null xs && null ys = res
     | null xs = res ++ ys
     | null ys = res ++ xs
     | head xs < head ys = helper (tail xs) ys (res ++ [head xs])
     | head xs == head ys = helper (tail xs) (tail ys) (res ++ [head xs])
     | otherwise = helper xs (tail ys) (res ++ [head ys])

removeDuplicates :: [Int] -> [Int] -> [Int]
removeDuplicates res xs
 | null xs = res
 | elem (head xs) res = removeDuplicates (tail xs) res
 | otherwise = removeDuplicates (tail xs) (res ++ [head xs])

mergeLinearXs :: [Int] -> [Int] -> [Int]
mergeLinearXs xs ys = sort $ removeDuplicates [] $ xs ++ ys