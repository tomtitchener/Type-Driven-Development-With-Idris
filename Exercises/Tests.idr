
module Tests

assertEq : (Eq a, Show a) => (name : String) -> (given : a) -> (expected : a) -> IO ()
assertEq name g e = if g == e
    then putStrLn $ "Test " ++ name ++ " Passed"
    else putStrLn $ "Test " ++ name ++ " Failed, Given " ++ (show g) ++ " Expected " ++ (show e)

assertNotEq : (Eq a, Show a) => (name : String) -> (given : a) -> (expected : a) -> IO ()
assertNotEq name g e = if not (g == e)
    then putStrLn $ "Test " ++ name ++ " Passed"
    else putStrLn $ "Test " ++ name ++ " Failed, Given " ++ (show g) ++ " Expected " ++ (show e)

