module Exercises_4_3_5

import Data.Vect
import Exercises.Tests

record DataStore where
  constructor MkData
  size  : Nat
  items : Vect size String
  
%name DataStore ds

data Command = Add String | Search String | Get Integer | Size | Quit

total addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newitem = MkData (S size) (addToItems items)
where
  addToItems : (items : Vect n String) -> Vect (S n) String
  addToItems [] = [newitem]
  addToItems (x :: xs) = x :: addToItems xs

total parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add"    str = Just (Add str)
parseCommand "search" str = Just (Search (trim str))
parseCommand "get"    val = case all isDigit (unpack val) of
                                 False => Nothing
                                 True => Just (Get (cast val))
parseCommand "size" _     = Just Size
parseCommand "quit" _     = Just Quit
parseCommand _ _          = Nothing

parseInput : (input : String) -> Maybe Command
parseInput input = case span (/= ' ') input of
                     (cmd, args) => parseCommand cmd (ltrim args)

getEntry : (pos : Integer) -> (store : DataStore) -> (input : String) -> Maybe (String, DataStore)
getEntry pos store input = let store_items = items store in
                                case integerToFin pos (size store) of
                                    Nothing => Just ("out of range\n", store)
                                    (Just id) => Just (index id store_items ++ "\n", store)

do_filter : String -> String -> Bool
do_filter targ src = any (== targ) $ words src

search_store : (store : DataStore) -> (targ : String) -> String
search_store store targ = let matches = filter (do_filter targ) (toList (items store)) in
                              unwords matches

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input =
  case parseInput input of
    Nothing            => Just ("invalid command\n", store)
    Just (Add    item) => Just ("ID " ++ (show (size store)) ++ "\n", addToStore store item)
    Just (Search targ) => Just (search_store store targ ++ "\n", store)
    Just (Get    item) => getEntry item store input
    Just (Size)        => Just (show (size store) ++ "\n", store)
    Just (Quit)        => Nothing

-- Test helpers
empty_ds    : DataStore
empty_ds    = MkData Z []
    
empty_pr : (String, DataStore)
empty_pr = ("", empty_ds)

m_pr_2_pr : Maybe (String, DataStore) -> (String, DataStore)
m_pr_2_pr m_pr = case m_pr of
                   Just (s,ds) => (s,ds)
                   Nothing     => empty_pr
-- Tests
test_ds_size : IO ()
test_ds_size = 
  assertEq "test_ds_size" 2 (size ds')
  where
    ds  : DataStore
    ds  = snd $ m_pr_2_pr $ processInput empty_ds "add One"
    ds' : DataStore
    ds' = snd $ m_pr_2_pr $ processInput ds "add Two"

test_ds_add_one : IO ()
test_ds_add_one =
  assertEq "test_ds_add_one" "One\n" $ ans
  where
    ds  : DataStore
    ds  = snd $ m_pr_2_pr $ processInput empty_ds "add One"
    ans : String
    ans = fst $ m_pr_2_pr $ processInput ds "get 0"
    
test_ds_search : IO ()
test_ds_search =
  assertEq "test_ds_search" gvn exp
  where
    gvn : String  
    gvn = "First a Line Second another Line\n"
    ds  : DataStore
    ds  = snd $ m_pr_2_pr $ processInput empty_ds "add First a Line"
    ds' : DataStore
    ds' = snd $ m_pr_2_pr $ processInput ds "add Second another Line"
    exp : String
    exp = fst $ m_pr_2_pr $ processInput ds' "search Line"
    
--  4.3.5  Exercises
--    Add a size command which displays the number of entries in the store.
--    Add a search command which displays all of the entries in the store containing a given substring.

main : IO ()
main = replWith (MkData Z []) "Command: " processInput
