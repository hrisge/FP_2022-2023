main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False 

-- ordered :: BTree a -> Bool
ordered Nil = True
ordered (Node (x, y) Nil Nil) = True
ordered (Node (x, y) left@(Node (xl, yl) lLeft lRight) Nil) = x < xl && y > yl && ordered left
ordered (Node (x, y) Nil right@(Node (xr, yr) rLeft rRight)) = x > xr && y < yr && ordered right
ordered (Node (x, y) left@(Node (xl, yl) lLeft lRight) right@(Node (xr, yr) rLeft rRight)) = x < xl && y > yl && x > xr && y < yr && ordered left && ordered right


data BTree a = Nil | Node a (BTree a) (BTree a)    
 deriving (Show, Eq)

t1 :: BTree (Int,Int)
t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

t2 :: BTree (Int,Int)
t2 = Node (3, 10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))