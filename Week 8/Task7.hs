main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True


isPerfect :: Integer -> Bool
isPerfect num 
 | num == 1 = False
 | otherwise = num == (foldl1 (+) (filter (\x -> (mod num x == 0)) [1..num-1]))  