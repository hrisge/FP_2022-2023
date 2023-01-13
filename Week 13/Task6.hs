main :: IO()
main = do
    print $ (getOddCompositionValue []) 2 == 2
    print $ (getOddCompositionValue [(\x -> x + 1),(\x -> x * 2),(\x -> x - 1), (\x -> div x 2)]) 2 == 2

getOddCompositionValue :: [(a -> a)] -> (a -> a)
getOddCompositionValue xs = (\ x -> foldr (\ (f, idx) acc -> if odd idx then f acc else acc) x $ zip xs [0 ..]) 