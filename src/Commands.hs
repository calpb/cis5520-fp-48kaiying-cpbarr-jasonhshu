module Commands where

import ShellInterpreter
import System.Process
import System.IO
import LuSyntax

valToString :: Value -> String
valToString (IntVal x) = show x 
valToString (BoolVal x) = show x 
valToString (StringVal x) = show x 
valToString x = show x 

runCommand :: Value -> [Value] -> Either IO ()
runCommand command args = do
    (_, Just hout, Just err, _) <- createProcess (proc (valToString command) (map valToString args)){ std_out = CreatePipe, std_err = CreatePipe }
    out <- hGetContents hout
    err <- hGetContents err
    if null err then Right out else Left err




