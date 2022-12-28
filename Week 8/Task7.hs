main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True


isPerfect :: Integer -> Bool
isPerfect num = num == sum [ c | c <- [1..num-1] , mod num c == 0 && num /= 1]