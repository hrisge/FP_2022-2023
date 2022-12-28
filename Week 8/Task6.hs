main :: IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 4 == False
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True
    

isPrimeLC :: Int -> Bool
isPrimeLC x = x /= 1 && (notElem True [0 == mod x c | c <- [2..x-1]]) 

isPrimeG :: Int -> Bool
isPrimeG x = helper x 2
 where
    helper :: Int -> Int -> Bool
    helper x y 
     | x <= 1 = False
     | x == y = True
     | 0 == mod x y = False
     | otherwise = helper x (y + 1)    
