module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)
  
mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch L value R = Branch (M (tsize L + tsize R + 1) ((max (tdepth L) (tdepth R)) + 1)) L value R

tsize :: Tree a -> Int
tsize Leaf = 0
tsize Branch (M size _) _ _ _ = size

tdepth :: Tree a -> Int
tdepth Leaf = 0
tdepth Branch (M _ depth) _ _ _ = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x Branch _ L value R
  | x < value   = tmember x L
  | x == value  = True
  | x > value   = tmember x R

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x @tree{Branch _ L value R}
  | x < value   = balance (mkBranch (tinsert x L) value R)
  | x == value  = tree
  | x > value   = balance (mkBranch L value (tinsert x R))

tFromList :: Ord a => [a] -> Tree a
tFromList [] = Leaf
tFromList [x:xs] = tinsert x (tFromList xs)

balance :: Ord a => Tree a -> Tree a
balance @tree{Branch _ L a (Branch _ B c R)} =
  if (tdepth B) - (tdepth L) == 2 && (tdepth C) <= (tdepth R)
  then rotateLeft tree
  else tree
balance @tree{Branch _ (Branch _ L b C) a R} =
  if (tdepth B) - (tdepth R) == 2 && (tdepth C) <= (tdepth L)
  then rotateRight tree
  else tree
balance @tree{Branch _ L a (Branch _ (Branch _ M c N) b R)} =
  if (tdepth B) - (tdepth L) == 2 && (tdepth C) > (tdepth R)
  then bigRotateLeft tree
  else tree
balance @tree{Branch _ (Branch _ L b (Branch _ M c N)) a R} =
  if (tdepth B) - (tdepth R) == 2 && (tdepth C) > (tdepth L)
  then bigRotateRight tree
  else tree
balance tree = tree

rotateLeft :: Ord a => Tree a -> Tree a
rotateLeft Branch _ L a (Branch _ B c R) = mkBranch (mkBranch L a C) b R

rotateRight :: Ord a => Tree a -> Tree a
rotateRight Branch _ (Branch _ L b C) a R = mkBranch L b (mkBranch C a R)

bigRotateLeft :: Ord a => Tree a -> Tree a
bigRotateLeft Branch _ L a (Branch _ (Branch _ M c N) b R) = mkBranch (mkBranch L a M) c (mkBranch N b R)

bigRotateRight :: Ord a => Tree a -> Tree a
bigRotateRight Branch _ (Branch _ L b (Branch _ M c N)) a R = mkBranch (mkBranch L b M) c (mkBranch N a R)