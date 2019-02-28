import Data.List

osort :: Ord a => Char -> [a] -> [a]
osort 'f' b = fwdSort b
osort 'b' b = backSort b
osort a b = []

-- fwdSort and backSort implementations based on: http://learnyouahaskell.com/recursion
fwdSort :: (Ord a) => [a] -> [a]  
fwdSort [] = []  
fwdSort (x:xs) =   
    let smallerSorted = fwdSort [a | a <- xs, a <= x]  
        biggerSorted = fwdSort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

backSort :: (Ord a) => [a] -> [a]
backSort [] = []
backSort (x:xs) =
    let smallerSorted = backSort [a | a <- xs, a <= x]
        biggerSorted = backSort [a | a <- xs, a > x]
    in  biggerSorted ++ [x] ++ smallerSorted

permute :: Ord a => [Int] -> [a] -> [a] 
permute [] [] = []
permute [] b = []
permute a [] = []
permute a b
    | maximum a > length b = []
    | otherwise            = [b !! (x - 1) | x <- a]

data Choose = Sort Char | 
   Permute [Int]
reorder :: (Ord a) => Choose -> [a] -> [a]
reorder (Sort a) b = osort a b
reorder (Permute a) b = permute a b