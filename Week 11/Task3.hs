main :: IO()
main = do
    print $ matching "1234" == []
    print $ matching ",[.[-],]" -- == [(1, 7), (3, 5)]
    print $ matching ",+[-.,+]" == [(2,7)]
    print $ matching "[][]" == [(0,1) , (2, 3)]

matching :: String -> [(Int, Int)]
matching = helper [] . zip [0 ..] 
 where
    helper _ [] = []
    helper stack ((indOp, '['):str) = helper (indOp:stack) str
    helper (s:stack) ((indCl, ']'):str) = (s,indCl):helper stack str
    helper stack (_ :str) = helper stack str

-- matching :: String -> [(Int, Int)]
-- matching [] = []
-- matching str = helper str 0 []
--  where
--     insertIndex :: [(Int, Int)] -> Int -> [(Int, Int)]
--     insertIndex (s:str) index 
--      | snd s == 0 = (fst s, index):str
--      | otherwise = [s] ++ insertIndex str index
--     helper :: [Char] -> Int -> [(Int, Int)] -> [(Int, Int)]
--     helper [] _ res = res
--     helper (s:str) index res 
--      | s == '[' = helper str (index + 1) (res ++ [(index, 0)])
--      | s == ']' = helper str (index + 1) (reverse $ insertIndex (reverse res) index)
--      | otherwise = helper str (index + 1) res