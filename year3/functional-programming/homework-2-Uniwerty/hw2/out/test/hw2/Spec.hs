import Test.QuickCheck
import HW2.T1

main :: IO ()
main = do
  quickCheck prop_equalSize
  
prop_equalSize :: Tree a -> Bool
prop_equalSize Leaf = treeToList Leaf == 0
prop_equalSize tree@(Branch size _ _ _) = treeToList tree == size

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []