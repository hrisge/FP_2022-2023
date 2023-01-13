main :: IO()
main = do
    print $ isGraceful t1 == True -- t1 = A
    print $ isGraceful t2 == True -- t2 = B
    print $ isGraceful t3 == False -- t3 = C

isGraceful :: Tree Int -> Bool
isGraceful Nil = True
isGraceful (Node value [Nil]) = True
isGraceful (Node value xs) = all (\ (Node v ys) -> even $ abs $ v - value) xs && all isGraceful xs

data Tree a = Nil | Node a [Tree a]    
 deriving (Show, Eq)

t1 :: Tree Int
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: Tree Int
t2 = Node 7 [Node 3 [Node 9 [Node 5 [Nil], Node 1 [Nil]]]]

t3 :: Tree Int
t3 = Node 1 [Node 3 [Nil], Node 5 [Node 42 [Nil]], Node 7 [Nil], Node 9 [Nil]]
