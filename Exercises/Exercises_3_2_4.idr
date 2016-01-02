module Exercises_3_2_4

import Data.Vect
import Exercises.Tests

total length : List a -> Nat
length = foldl (const . S) Z

test_zero_len : IO ()
test_zero_len = assertEq "test_zero_len" (Exercises_3_2_4.length {a=Integer} []) 0

test_non_zero_len : IO ()
test_non_zero_len = assertEq "test_non_zero_len" (Exercises_3_2_4.length [1..10]) 10

reverse : List a -> List a
reverse = foldl (flip (::)) []

test_reverse : IO ()
test_reverse = assertEq "test_reverse" (Exercises_3_2_4.reverse [1..5]) [5,4,3,2,1]

mapL : (a -> b) -> List a -> List b
mapL f [] = []
mapL f (l :: ls) = f l :: mapL f ls

test_mapL : IO ()
test_mapL = assertEq "test_mapL" (mapL (+1) [1,2,3]) [2,3,4]

mapV : (a -> b) -> Vect n a -> Vect n b
mapV f [] = []
mapV f (v :: vs) = f v :: mapV f vs

test_mapV : IO ()
test_mapV = assertEq "test_mapV" (mapL (+1) [1,2,3]) [2,3,4]
