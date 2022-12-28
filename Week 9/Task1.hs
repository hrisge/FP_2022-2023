main :: IO()
main = do
    print $ addN [1, 2, 3, 4, 5] 9999999999999999999999 == [10000000000000000000000,10000000000000000000001,10000000000000000000002,10000000000000000000003,10000000000000000000004]
    print $ sqAddN [1, 2, 3, 4, 5] 5 == [36,49,64,81,100]
    print $ divByN [1, 2, 3, 4, 5] 5 == [0.2,0.4,0.6,0.8,1.0]
    print $ filterByN [1, 2, 3, 4, 5] 3 == [3,4,5]

addN :: [Integer] -> Integer -> [Integer]
addN xs y = map (\x -> x + y) xs    

sqAddN :: [Integer] -> Integer -> [Integer]
sqAddN xs y = map (\x -> x * x) $ addN xs y

divByN :: [Int] -> Int -> [Double]
divByN xs y
 | y <= 0 = []
 | otherwise = map (\x -> (fromIntegral x) / (fromIntegral y)) xs

filterByN :: [Int] -> Int -> [Int]
filterByN xs y = filter (\x -> x >= y) xs