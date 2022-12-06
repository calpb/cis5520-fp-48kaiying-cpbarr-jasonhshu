module Commands where

import ShellSyntax
import System.IO
import System.Process

runLS :: IO ()
runLS = do
  (_, Just hout, Just err, _) <- createProcess (proc "ls" ["~"]) {std_out = CreatePipe, std_err = CreatePipe}
  dateOut <- hGetContents hout
  stderr <- hGetContents err
  putStrLn "> date"
  if null stderr then putStrLn dateOut else putStrLn stderr

valToString :: Value -> String
valToString (IntVal x) = show x
valToString (BoolVal x) = show x
valToString (StringVal x) = show x
valToString x = show x

runCommand :: Value -> [Value] -> Either IO ()
runCommand command args = do
  (_, Just hout, Just err, _) <- createProcess (proc (valToString command) (map valToString args)) {std_out = CreatePipe, std_err = CreatePipe}
  out <- hGetContents hout
  err <- hGetContents err
  if null stderr then Right dateOut else Left stderr
