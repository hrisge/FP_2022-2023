main :: IO()
main = do
    print $ addN [1, 2, 3, 4, 5] 9999999999999999999999 == [10000000000000000000000,10000000000000000000001,10000000000000000000002,10000000000000000000003,10000000000000000000004]
    print $ sqAddN [1, 2, 3, 4, 5] 5 == [36,49,64,81,100]
    print $ divByN [1, 2, 3, 4, 5] 5 == [0.2,0.4,0.6,0.8,1.0]
    print $ divByN [1, 2, 3, 4, 5] (-5) == [-0.2,-0.4,-0.6,-0.8,-1.0]
    print $ filterByN [1, 2, 3, 4, 5] 3 == [3,4,5]

addN :: (Num a) => [a] -> a -> [a]
addN xs y = map (+y) xs

sqAddN :: (Num a) => [a] -> a -> [a]
sqAddN xs y = map (^2) $ map (+y) xs

divByN :: [Int] -> Int -> [Double]
divByN xs 0 = error "Can not divide by 0"
divByN xs y = map (\x -> (fromIntegral x) / (fromIntegral y)) xs

filterByN :: (Ord a, Num a) => [a] -> a -> [a]
filterByN xs y = filter (\x -> x >= y) xs