import Data.Char

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

isDuplicate :: Char -> Char -> Bool
isDuplicate x y = (((isLower x && isUpper y)) || (isUpper x && isLower y)) && (toUpper x == toUpper y)

reduceStr :: [Char] -> [Char]
reduceStr str = (helper str [])
 where 
    helper :: [Char] -> [Char] -> [Char]
    helper leftover result
     | length leftover == 1 = result ++ [(head leftover)]
     | null leftover = result
     | isDuplicate (head leftover) (head (tail leftover)) = helper (result ++ (tail (tail leftover))) []
     | otherwise = helper (tail leftover) (result ++ [head leftover])                                                           
