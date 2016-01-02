module Exercises_4_2_4

import Data.Vect
import Prelude.Maybe
import Prelude.Monad
import Exercises.Tests

{-
1) Extend the Vehicle data type so that it supports unicycles and motorcycles,
   and update wheels and refuel accordingly.
2) Extend the PowerSource and Vehicle data types to support electric vehicles
  (such as trams, or electric cars).
-}

data PowerSource = Pedal | Petrol | Electric

data Vehicle : PowerSource -> Type where
  Bicycle    : Vehicle Pedal
  Unicycle   : Vehicle Pedal
  MotorCycle : (fuel : Nat)  -> Vehicle Petrol
  Car        : (fuel : Nat)  -> Vehicle Petrol
  Bus        : (fuel : Nat)  -> Vehicle Petrol
  Trams      : Vehicle Electric

total wheels : Vehicle _ -> Nat
wheels Bicycle           = 2
wheels Unicycle          = 1
wheels (MotorCycle fuel) = 2
wheels (Car fuel)        = 4
wheels (Bus fuel)        = 4
wheels Trams             = 8

total refuel : Vehicle Petrol -> Vehicle Petrol
refuel (MotorCycle fuel) = MotorCycle 10
refuel (Car fuel)        = Car 100
refuel (Bus fuel)        = Bus 200

{-
3) The take function, on List, has type Nat -> List a -> List a.
   What is an appropriate type for the corresponding function on Vect?
   (Hint: How do the lengths of the input and output relate?
    Remember that you can have any expression in a type!)
4) Implement the take function on Vect.
-}
take : (f : Fin (S n)) -> Vect n a -> Vect (cast f) a
take FZ xs = []
take (FS f) (x :: xs) = x :: take f xs

test_take_vect : IO ()
test_take_vect =
  assertEq "test_take_vect" v (Exercises_4_2_4.take 3 [1,2,3,4,5,6])
  where
    v : (Vect 3 Integer)
    v = [1,2,3]

{-
5) Write a function sumEntries with the following type:
   sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
   It should return the sum of the entries at position pos in each of the inputs,
   if pos is within bounds, or Nothing otherwise.
   For example:
   *ch04_exercises> sumEntries 2 [1,2,3,4] [5,6,7,8]
   Just 10 : Maybe Integer
   *ch04_exercises> sumEntries 4 [1,2,3,4] [5,6,7,8]
   Nothing : Maybe Integer
   (Hint: You'll need to call integerToFin, but you'll only need to do it once!)
-}

sumEntries : Num a => (i : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} i xs ys = integerToFin i n >>= (\x => Just $ index x xs + index x ys)

test_sum_entries_ok : IO ()
test_sum_entries_ok =
  assertEq "test_sum_entries_ok" (Just 10) $ sumEntries 2 v1 v2
  where
    v1 : Vect 4 Integer
    v1 = [1,2,3,4]
    v2 : Vect 4 Integer
    v2 = [5,6,7,8]
    
test_sum_entries_fail : IO ()
test_sum_entries_fail =
  assertEq "test_sum_entries_fail" Nothing $ sumEntries 4 v1 v2
  where
    v1 : Vect 4 Integer
    v1 = [1,2,3,4]
    v2 : Vect 4 Integer
    v2 = [5,6,7,8]
    
  
