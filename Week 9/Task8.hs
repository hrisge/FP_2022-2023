import Data.Char
import Data.List

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

isDuplicate :: Char -> Char -> Bool
isDuplicate x y = (((isLower x && isUpper y)) || (isUpper x && isLower y)) && (toUpper x == toUpper y)

reduceStr :: [Char] -> [Char]
reduceStr str = reverse (helper str [])
 where 
    helper :: [Char] -> [Char] -> [Char]
    helper [] result = result
    helper (l:leftover) [] = helper leftover [l]
    helper (l:leftover) (r:result)
     | not $ isDuplicate l r = helper leftover ([l] ++ [r] ++ result)
     | isDuplicate l r = helper leftover result                                                             
