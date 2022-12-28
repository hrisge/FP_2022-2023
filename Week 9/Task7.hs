import Data.Char

main :: IO()
main = do
    print $ duplicateCount "" == 0 -- no characters repeats more than once
    print $ duplicateCount "abcde" == 0
    print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
    print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
    print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
    print $ duplicateCount "Indivisibility" == 1
    print $ duplicateCount "aA11" == 2 -- 'a' and '1'
    print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
    print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
    print $ duplicateCount ['a'..'z'] == 0
    print $ duplicateCount (['a'..'z'] ++ ['A'..'Z']) == 26


firstElementHasDuplicates :: [Char] -> Bool
firstElementHasDuplicates xs = elem (head xs) (tail xs)

removeElement :: [Char] -> Char -> [Char]
removeElement xs x = helper xs x []
 where
    helper :: [Char] -> Char -> [Char] -> [Char]
    helper xs x res
     | null xs = res
     | head xs /= x = helper (tail xs) x (res ++ [head xs])
     | otherwise = helper (tail xs) x res

duplicateCount :: [Char] -> Int
duplicateCount str = helper (map toLower str) 0
 where 
    helper :: [Char] -> Int -> Int
    helper str res
     | null str = res
     | firstElementHasDuplicates str = helper (removeElement str (head str)) (res + 1)
     | otherwise = helper (tail str) res
