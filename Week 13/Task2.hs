main :: IO()
main = do
    print $ colourBTree
    print $ highest Red colourBTree == 4
    print $ highest Green colourBTree == 3
    print $ highest Blue colourBTree == 4

highest :: Colour -> BTree Colour -> Int
highest col tree = helper tree 1
 where
    helper :: BTree Colour -> Int -> Int
    helper Nil _ = 0
    helper (Node value left right) ind
     | value == col = max (max ind (helper left (ind + 1))) (helper right (ind + 1))
     | otherwise = max (helper left (ind + 1)) (helper right (ind + 1))

data Colour = Red | Green | Blue
 deriving (Show, Eq)

data BTree a = Nil | Node a (BTree a) (BTree a)    
 deriving (Show, Eq)

colourBTree :: BTree Colour
colourBTree = Node Blue (Node Green (Node Blue (Node Red Nil Nil) Nil) (Node Blue Nil Nil)) (Node Red (Node Green (Node Blue Nil Nil) Nil) (Node Red Nil Nil))     