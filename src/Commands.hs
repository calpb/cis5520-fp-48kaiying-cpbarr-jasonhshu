module Commands where 

import Logic 
import Test.HUnit


echo :: (Input m, Output m) => m ()
echo = do
  ms <- input
  case ms of
    Just str -> write str >> write "\n"
    Nothing -> echo


testEcho :: Test
testEcho =
  runFakeIO
    echo
    [Nothing, Nothing, Just "hello"]
    ~?= ["hello", "\n"]

-- >>> runTestTT testEcho
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

testEcho2 :: Test
testEcho2 =
  runFakeIO
    (echo >> echo)
    [Just "hello", Nothing, Just "CIS 5520"]
    ~?= ["hello", "\n", "CIS 5520", "\n"]

-- >>> runTestTT testEcho2
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
