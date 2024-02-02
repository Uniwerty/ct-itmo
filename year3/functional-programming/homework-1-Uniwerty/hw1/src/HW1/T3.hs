module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int
  deriving (Show)

-- AVL-tree
data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l value r = Branch (M (tsize l + tsize r + 1) ((max (tdepth l)  (tdepth r)) + 1)) l value r

tsize :: Tree a -> Int
tsize Leaf                      = 0
tsize (Branch (M size _) _ _ _) = size

tdepth :: Tree a -> Int
tdepth Leaf                       = 0
tdepth (Branch (M _ depth) _ _ _) = depth

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember x (Branch _ l value r)
  | x < value   = tmember x l
  | x == value  = True
  | x > value   = tmember x r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x Leaf = mkBranch Leaf x Leaf
tinsert x tree@(Branch _ l value r)
  | x < value   = balance (mkBranch (tinsert x l) value r)
  | x == value  = tree
  | x > value   = balance (mkBranch l value (tinsert x r))

tFromList :: Ord a => [a] -> Tree a
tFromList []     = Leaf
tFromList (x:xs) = tinsert x (tFromList xs)

balance :: Tree a -> Tree a

-- left rotate
balance (Branch _ l a treeB@(Branch _ c b r))
  | (tdepth treeB) - (tdepth l)  == 2 && (tdepth c) <= (tdepth r) =
      mkBranch (mkBranch l a c) b r

-- right rotate
balance (Branch _ treeB@(Branch _ l b c) a r)
  | (tdepth treeB) - (tdepth r) == 2 && (tdepth c) <= (tdepth l) =
      mkBranch l b (mkBranch c a r)

-- big left rotate
balance (Branch _ l a treeB@(Branch _ treeC@(Branch _ m c n) b r))
  | (tdepth treeB) - (tdepth l)  == 2 && (tdepth treeC) > (tdepth r) =
      mkBranch (mkBranch l a m) c (mkBranch n b r)

-- big right rotate
balance (Branch _ treeB@(Branch _ l b treeC@(Branch _ m c n)) a r)
  | (tdepth treeB) - (tdepth r) == 2 && (tdepth treeC) > (tdepth l) =
      mkBranch (mkBranch l b m) c (mkBranch n a r)

-- otherwise
balance tree = tree
