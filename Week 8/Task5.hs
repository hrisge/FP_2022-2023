main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False


hasIncDigits :: Int -> Bool
hasIncDigits 0 = True
hasIncDigits x = mod x 10 >= mod (div x 10) 10 && hasIncDigits (div x 10)