module Exercises_4_1_5

import Exercises.Tests

data BSTree : Type -> Type where
  Empty : Ord a => BSTree a
  Node  : Ord a => (left : BSTree a) -> (val : a) -> (right : BSTree a) -> BSTree a

-- naive/unbalanced
total insert : Ord a => a -> BSTree a -> BSTree a
insert v Empty = Node Empty v Empty
insert v node@(Node left val right) = case compare v val  of
                                      LT => Node (insert v left) val right
                                      EQ => node
                                      GT => Node left val (insert v right)

--Write a function listToTree : Ord a => List a -> Tree a
-- which inserts every element of a list into a binary search tree.

total listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

--Write a corresponding function treeToList : Tree a -> List a
-- which flattens a tree into a list using inorder traversal
-- (i.e. all the values in a left subtree of a node should be
-- be added to the list before the value at the node,
-- which should be added before the values in the right subtree).

total treeToList : Ord a => BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

test_tree_list : IO ()
test_tree_list = assertEq "test_tree_list" [1..10] $ (treeToList . listToTree) [1,5,4,3,2,10,8,9,6,7]

-- An integer arithmetic expression can take one of the following forms:
-- A single integer
-- Addition of an expression to an expression
-- Subtraction of an expression from an expression
-- Multiplication of an expression with an expression
-- Define a recursive data type Expr which can be used to represent such expressions.
-- (Hint: Look at the Picture data type and see how the informal description mapped to the data declaration.)
-- Write a function evaluate : Expr -> Int which evaluates an integer arithmetic expression.

-- Generalize.  Naïve impl runs into trouble with Nat as an instance of Num and 'a - b' for Nat requring a > b.  
-- Is there a Num equivalent for signed numbers where Nat wouldn't be a member?  Yes:  Neg.  

data Expr : Type -> Type where
  Val  : Neg a => (val : a) -> Expr a
  Sum  : Neg a => Expr a -> Expr a -> Expr a
  Diff : Neg a => Expr a -> Expr a -> Expr a
  Prod : Neg a => Expr a -> Expr a -> Expr a

total eval : Neg a => Expr a -> a
eval (Val val)  = val
eval (Sum x y)  = (eval x) + (eval y)
eval (Diff x y) = (eval x) - (eval y)
eval (Prod x y) = (eval x) * (eval y)

test_expr_eval : IO ()
test_expr_eval =
  assertEq "test_expr_eval" 8 $ eval (Sum (Prod (Val 5) (Val 1)) (Diff (Val 4) (Val 1)))

-- Write a function biggestTriangle : Picture -> Maybe Double which returns the area of
-- the biggest triangle in a picture.
