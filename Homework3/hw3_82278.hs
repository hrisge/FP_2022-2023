main :: IO()
main = do
    print $ generate [10, 15, 25] [1, 5, 20, 30] == [[10, 20], [10, 20, 25, 30], [10, 30], [15, 20], [15, 20, 25, 30], [15, 30], [25, 30]]

generate :: [Int] -> [Int] -> [[Int]]
generate as bs = helper as bs bs as bs [] []
 where
    helper :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [[Int]] -> [Int] -> [[Int]]
    helper [] bs bsStart a2s b2s res curr = res
    helper (a:as) bs [] a2s b2s res curr = helper as bs bs as bs res [] 
    helper as bs (b:bsStart) a2s [] res curr = helper as bs bsStart as bsStart res []
    helper as bs (b:bsStart) [] b2s res curr = helper as bs bsStart as bsStart res []
    helper as bs (start:bsStart) (a:a2s) (b:b2s) res []
     | a < b = helper as bs ([b] ++ bsStart) a2s b2s (res ++ [[a] ++ [b]]) ([a] ++ [b])
     | otherwise = helper as bs bsStart ([a] ++ a2s) b2s res []
    helper as bs bsStart (a:a2s) (b:b2s) res curr
     | a < b && (last curr) < a = helper as bs bsStart a2s b2s (res ++ [curr ++ [a] ++ [b]]) (curr ++ [a] ++ [b])
     | (last curr) >= a = helper as bs bsStart a2s ([b] ++ b2s) res curr
     | otherwise = helper as bs bsStart ([a] ++ a2s) b2s res curr 
 