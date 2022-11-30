module Logic where 


import Control.Monad (ap, liftM, (>=>))
import qualified Control.Monad.State as S
import Control.Monad.Trans (MonadTrans (..))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified System.IO as IO
import Test.HUnit (Test, runTestTT, (~?=))

-- | FakeIO utilities 

class Monad m => Output m where
  write :: String -> m ()

class Monad m => Input m where
  input :: m (Maybe String) -- only return input if it is ready

instance Output IO where
  write = putStr

instance Input IO where
  input = do
    x <- IO.hReady IO.stdin
    if x then Just <$> getLine else return Nothing

type FakeIO = S.State FakeState

data FakeState = FS
  { fsWrite :: Seq String, -- what has been written
    fsInput :: [Maybe String] -- what to read from
  }

instance Output FakeIO where
    write s = do
    st <- S.get
    let oldLog = fsWrite st
    let newLog = oldLog <> Seq.singleton s
    S.put $ st {fsWrite = newLog}

instance Input FakeIO where
    input = do
    st <- S.get
    let (x:xs) = fsInput st
    S.put $ st {fsInput = xs}
    return x

runFakeIO :: FakeIO () -> [Maybe String] -> [String]
runFakeIO comp inputs =
  toList (fsWrite (S.execState comp initState))
  where
    initState = FS {fsWrite = Seq.empty, fsInput = inputs}
