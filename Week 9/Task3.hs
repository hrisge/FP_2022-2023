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
mergeLinearRec xs ys = nub (helper xs ys [])
 where 
    helper :: [Int] -> [Int] -> [Int] -> [Int]
    helper [] [] res = res
    helper [] ys res = res ++ ys
    helper xs [] res = res ++ xs
    helper (x:xs) (y:ys) res
     | x <= y = helper xs ([y] ++ ys) (res ++ [x])
     | otherwise = helper ([x] ++ xs) ys (res ++ [y])

mergeLinearXs :: [Int] -> [Int] -> [Int]
mergeLinearXs xs ys = sort $ nub $ xs ++ ys