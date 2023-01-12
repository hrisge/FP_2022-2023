main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)

    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)

    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True

type Rat = (Int, Int)


sumRats :: Rat -> Rat -> Rat 
sumRats (x1, y1) (x2, 0) = error "Can not divide by 0" 
sumRats (x1, 0) (x2, y2) = error "Can not divide by 0"
sumRats (x1, y1) (0, y2) = error "Can not divide by 0"
sumRats (x1, y1) (x2, y2) = (div (x1 * y2 + x2 * y1) (gcd (x1 * y2 + x2 * y1) (y1 * y2)), div (y1 * y2) (gcd (x1 * y2 + x2 * y1) (y1 * y2)))

multiplyRats :: Rat -> Rat -> Rat
multiplyRats (x1, y1) (x2, 0) = error "Can not divide by 0" 
multiplyRats (x1, 0) (x2, y2) = error "Can not divide by 0"
multiplyRats (x1, y1) (0, y2) = error "Can not divide by 0"
multiplyRats (x1, y1) (x2, y2) = (div (x1 * x2) (gcd (x1 * x2) (y1 * y2)) , div (y1 * y2) (gcd (x1 * x2) (y1 * y2)))

divideRats :: Rat -> Rat -> Rat
divideRats (x1, y1) (x2, 0) = error "Can not divide by 0" 
divideRats (x1, 0) (x2, y2) = error "Can not divide by 0"
divideRats (x1, y1) (0, y2) = error "Can not divide by 0"
divideRats (x1, y1) (x2, y2) = multiplyRats (x1, y1) (y2, x2)

areEqual :: Rat -> Rat -> Bool
areEqual (x1, y1) (x2, 0) = error "Can not divide by 0" 
areEqual (x1, 0) (x2, y2) = error "Can not divide by 0"
areEqual (x1, y1) (0, y2) = error "Can not divide by 0"
areEqual (x1, y1) (x2, y2) = (div x1 (gcd x1 y1)) == (div x2 (gcd x2 y2)) && (div y1 (gcd x1 y1)) == (div y2 (gcd x2 y2))