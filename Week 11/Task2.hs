main :: IO()
main = do
    print $ stockList [ Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600] ['A', 'B'] == [('A', 200), ('B', 1140)]
    print $ stockList [ Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600] ['C', 'X'] == [('C', 500), ('X', 0)]
    print $ stockList [ Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600] ['Y', 'X'] == [('Y', 0), ('X', 0)]
    print $ stockList [ Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600] ['C'] == [('C', 500)] 

stockList :: [Stock] -> [Char] -> [(Char, Int)]
stockList n = map (\ ch1 -> (ch1, sum [ q | (Stock (firstL:_) q) <- n, firstL == ch1]))

-- stockList :: [Stock] -> [Char] -> [(Char, Int)]
-- stockList stock toTake = [find (stocksToPair stock) el (el, 0) | el <- toTake]
--  where 
--     stocksToPair stock = [(x, quantity) | (Stock (x:xs) quantity) <- stock]
--     find [] _ res = res
--     find ((x1, x2):xs) ch res@(r1, r2)
--      | x1 == ch = find xs ch (ch, r2 + x2)
--      | otherwise = find xs ch res

data Stock = Stock String Int    