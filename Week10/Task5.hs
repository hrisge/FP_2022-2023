main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

sortList :: [(Int, Int)] -> [(Int, Int)]
sortList xs = helper xs []
 where 
    helper :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
    helper [] res = res
    helper (x:xs) ys = helper xs (ys ++ [(min (fst x) (snd x), max (fst x) (snd x))])

combine :: [(Int, Int)] -> (Int, Int)
combine xs = foldl1 (\x y -> (10 * (fst x) + (fst y), 10 * (snd x) + (snd y)) ) (sortList xs)
