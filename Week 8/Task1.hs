main :: IO()
main = do
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5

sqAvg :: Int -> Int -> Double
sqAvg x y = (fromIntegral $ x * x + y * y) / 2    