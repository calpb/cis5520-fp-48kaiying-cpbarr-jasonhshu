module Commands where

--  import Logic

import System.Process as SP
import Test.HUnit

callProcess :: String -> IO ()
callProcess s = do
  -- output <- SP.shell s
  r1 <- SP.createProcess (proc "ls" [])
  r <- SP.createProcess (proc s [])
  return ()

-- putStrLn output

-- error handling in case shell returns an invalid result, or
-- simply just print out whatever we get

-- echo :: (Input m, Output m) => m ()
-- echo = do
--   ms <- input
--   case ms of
--     Just str -> write str >> write "\n"
--     Nothing -> echo

-- testEcho :: Test
-- testEcho =
--   runFakeIO
--     echo
--     [Nothing, Nothing, Just "hello"]
--     ~?= ["hello", "\n"]

-- >>> runTestTT testEcho
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-- testEcho2 :: Test
-- testEcho2 =
--   runFakeIO
--     (echo >> echo)
--     [Just "hello", Nothing, Just "CIS 5520"]
--     ~?= ["hello", "\n", "CIS 5520", "\n"]

-- >>> runTestTT testEcho2
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}
