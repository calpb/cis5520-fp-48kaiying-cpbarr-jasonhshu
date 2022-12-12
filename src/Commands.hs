{-# LANGUAGE ImportQualifiedPost #-}

module Commands where

import ShellSyntax
import System.IO
import System.Process
import System.Directory
import System.Directory.Internal.Prelude (exitFailure)
import Control.Exception

data MyException
  = ErrNoBin String
  | ErrRuntimeErr String
  deriving Show

instance Exception MyException

valToString :: Value -> String
valToString (IntVal x) = show x
valToString (BoolVal x) = show x
valToString (StringVal x) = x

execCmd :: Value -> [Value] -> IO String
execCmd command args = do
  path <- findExecutable (valToString command)
  case path of 
    Nothing -> error $ "No such binary found: " ++ valToString command
    Just _  -> do
      (_, Just hout, Just err, _) <- createProcess (proc (valToString command) (map valToString args)) {std_out = CreatePipe, std_err = CreatePipe}
      out <- hGetContents hout
      err <- hGetContents err
      if null err then return out else error err

execCmd' :: String -> [String] -> IO String
execCmd' command args = do
  path <- findExecutable command
  case path of 
    Nothing -> error $ "No such binary found: " ++ command
    Just _  -> do
      (_, Just hout, Just err, _) <- createProcess (proc command args) {std_out = CreatePipe, std_err = CreatePipe}
      out <- hGetContents hout
      err <- hGetContents err
      if null err then return out else error err

-- runCmd :: Value -> [Value] -> String 
-- runCmd command args = do
--   hClose stdout
--   catch (
--     do 
--       execCmd command args 
--    )
--    (\case
--       ErrNoBin x ->
--         hPutStrLn stderr x
--       ErrRuntimeErr x ->
--         hPutStrLn stderr x
--    )

date :: IO ()
date = do 
  (_, Just hout, _, _) <- createProcess (proc "date" []){ std_out = CreatePipe }
  dateOut <- hGetContents hout
  putStrLn "> date"
  putStrLn dateOut



-- >>> date


-- >>> execCmd (StringVal "ls") [(StringVal "+")]
-- No such binary found: "ls"


-- >>> execCommand (StringVal "expr") [(IntVal 3), (StringVal "+"), (IntVal 4)]
-- "expr": createProcess: posix_spawnp: does not exist (No such file or directory)
