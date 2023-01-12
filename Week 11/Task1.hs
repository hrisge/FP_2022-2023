import Data.Char

main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == -811181

squareDigits :: Int -> Int
squareDigits num
 | num < 0 = -squareDigits (-num)
 | otherwise = read $ concat [show $ digitToInt c * digitToInt c | c <- show num]