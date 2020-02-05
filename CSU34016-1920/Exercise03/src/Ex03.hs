{- mbungad Divine Mbunga -}
module Ex03 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !

-- Binary Tree
data BT a b
  = Leaf
  | Branch (BT a b) a b (BT a b)
  deriving (Eq, Show)

-- association list
type Assoc a b = [(a,b)]

-- lookup binary (search) tree
lkpBST :: Ord a1 => BT a1 a -> a1 -> Maybe a
lkpBST Leaf _  =  Nothing
lkpBST (Branch left k d right) k'
 | k < k'     =  lkpBST left k'
 | k > k'     =  lkpBST right k'
 | otherwise  =  Just d

-- Coding Part 1 (13 Marks)

-- insert into binary (search) tree
insBST :: Ord a => a -> b -> BT a b -> BT a b
insBST x y Leaf   = Branch Leaf x y Leaf

insBST x y (Branch left x' y' right) 
 |x == x' = Branch left x y right
 |x > x'  = Branch (left) x' y'(insBST x y right)
 |x < x'  = Branch (insBST x y left) x' y' (right)

insBST x y (Branch Leaf x' y' Leaf) 
 |x == x' = Branch Leaf x y Leaf 
 |x > x'  = Branch (Leaf) x' y'(Branch Leaf x y Leaf) 
 |x < x'  = Branch (Branch Leaf x y Leaf) x' y' (Leaf)
--insBST _ _ _  =  error "insBST not yet implmented"


-- Coding Part 2 (6 Marks)

-- convert an association list to a binary search tree
assoc2bst :: Ord a => Assoc a b -> BT a b
assoc2bst [] = Leaf
assoc2bst [(a,b)] = insBST a b Leaf
assoc2bst ((x,x'):xs) = 
          let y = assoc2bst xs
              in insBST x x' y
--assoc2bst _ = error "assoc2bst not yet implemented"

-- Coding Part 3 (6 Marks)

-- convert a binary search tree into an (ordered) association list
bst2assoc :: Ord c =>  BT c e -> Assoc c e
bst2assoc Leaf = []
bst2assoc (Branch Leaf c e Leaf) = [(c,e)] 
bst2assoc (Branch left x' y' right) = 
          let leftB = bst2assoc left
              rightB = bst2assoc right
              in (leftB)++([(x',y')])++(rightB)
   
--bst2assoc _ = error "bst2assoc not yet implemented"



