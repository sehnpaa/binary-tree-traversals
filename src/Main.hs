module Main where

data Tree a = Leaf a | Branch (Tree a) a (Tree a)

t :: Num a => Tree a
t = Branch (Branch (Leaf 4) 2 (Leaf 5)) 1 (Leaf 3)

-- preorder t: [1,2,4,5,3]
preorder :: Tree a -> [a]
preorder (Leaf a) = [a]
preorder (Branch left a right) = a : preorder left ++ preorder right

-- postorder t: [4,5,2,3,1]
postorder :: Tree a -> [a]
postorder (Leaf a) = [a]
postorder (Branch left a right) = postorder left ++ postorder right ++ [a]

-- inorder t: [4,2,5,1,3]
inorder :: Tree a -> [a]
inorder (Leaf a) = [a]
inorder (Branch left a right) = inorder left ++ a : inorder right

-- breathFirst t: [1,2,3,4,5]
breathFirst :: Tree a -> [a]
breathFirst (Leaf a) = [a]
breathFirst (Branch (Leaf l) a (Leaf r)) = a : l : r : []
breathFirst (Branch (Branch left l right) a (Leaf r)) =
    a : l : r : breathFirst left ++ breathFirst right
breathFirst (Branch (Leaf l) a (Branch left r right)) =
    a : l : r : breathFirst left ++ breathFirst right
breathFirst (Branch (Branch left l right) a (Branch left' r right')) =
    a : l : r : breathFirst left ++ breathFirst right ++ breathFirst left' ++ breathFirst right'

main :: IO ()
main = putStrLn "Hello, Haskell!"
