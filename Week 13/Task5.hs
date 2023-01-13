main :: IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]

listLeaves :: [(Int, Int, Int)] -> [Int]
listLeaves xs = helper xs []
 where
    helper :: [(Int, Int, Int)] -> [Int] -> [Int]
    helper ((x,y,z):xs) res
     elem x res = (removeItem res x) ++ [y] ++ [z]
     otherwise = res ++ [y] + [z]   

removeItem :: (Eq a) => [a] -> a -> [a]
removeItem [] _ = []
removeItem (x:xs) el 
 | x == el = removeItem xs el
 | otherwise = x : removeItem xs el
