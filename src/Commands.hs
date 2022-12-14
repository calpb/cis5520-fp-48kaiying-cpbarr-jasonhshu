{-# LANGUAGE ImportQualifiedPost #-}

module Commands where

import ShellSyntax
import System.Directory
import System.IO
import System.Process

valToString :: Value -> String
valToString (IntVal x) = show x
valToString (BoolVal x) = show x
valToString (StringVal x) = x

execCmd :: Value -> [Value] -> IO String
execCmd command args = do
  path <- findExecutable (valToString command)
  case path of
    Nothing -> error $ "No such binary found: " ++ valToString command
    Just _ -> do
      (_, Just hout, Just err, _) <- createProcess (proc (valToString command) (map valToString args)) {std_out = CreatePipe, std_err = CreatePipe}
      out <- hGetContents hout
      err <- hGetContents err
      if null err then return out else error err
