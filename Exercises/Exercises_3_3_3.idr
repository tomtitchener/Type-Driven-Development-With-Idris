module Exercises_3_3_3

import Data.Vect
import Exercises.Tests

--Reimplement transpose_mat using zipWith instead of transpose_helper.

total xpose_mat : Vect m (Vect n a) -> Vect n (Vect m a)
xpose_mat {n} [] = replicate n []
xpose_mat (x::xs) = zipWith (::) x (xpose_mat xs)

--Implement addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)

total add_mat : (Num a) => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
add_mat [] [] = []
add_mat (x :: xs) (y :: ys) = zipWith (+) x y :: add_mat xs ys

test_double_mat : IO ()
test_double_mat =
  assertEq "test_double_mat" (add_mat m m) ((map . map) (*2) m)
  where
    m : Vect 2 (Vect 3 Integer) -- required!
    m = [[1,2,3],[4,5,6]]

{-
Implement a function for multiplying matrices, following the description given in Section  
“Matrix Operations and Their Types”.
Hints: This definition is quite tricky and involves multiple steps! Consider the following:
You have a left matrix, of dimensions n x m, and a right matrix of dimensions m x p.
A good start is to use transpose_mat on the right matrix.
Remember that you can use Ctrl-Alt-L to lift holes to top level functions.
Remember to pay close attention to the types of the local variables and the types of the holes.
Remember to use Ctrl-Alt-S to search for expressions, and pay close attention to the types of any resulting holes.
-}

dot_prod : Num a => Vect m a -> Vect m a -> a
dot_prod xs ys = sum $ zipWith (*) xs ys

mult_mat_inner : Num a => Vect n (Vect m a) -> Vect m a -> Vect n a
mult_mat_inner yss xs = map (dot_prod xs) yss

total mult_mat : (Num a) => Vect l (Vect m a) -> Vect m (Vect n a) -> Vect l (Vect n a)
mult_mat xss yss = map (mult_mat_inner (xpose_mat yss)) xss

test_mult_mat : IO ()
test_mult_mat =
  assertEq "test_mult_mat" (mult_mat xss yss) zss
  where
    xss : Vect 3 (Vect 2 Integer)
    xss = [[1,2],[3,4],[5,6]]
    yss : Vect 2 (Vect 4 Integer)
    yss = [[7,8,9,10],[11,12,13,14]]
    zss : Vect 3 (Vect 4 Integer)
    zss = [[29,32,35,38],[65,72,79,86],[101,112,123,134]]
