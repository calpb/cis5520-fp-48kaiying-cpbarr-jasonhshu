module ShellInterpreter where

import Commands
import Commands qualified
import Control.Monad (unless, when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (Maybe (Nothing), fromMaybe)
import GHC.Base (undefined)
import GHC.Real (underflowError)
import ShellParser qualified as P
import ShellSyntax
import State (State)
import State qualified as S
import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC
import Text.Read (readMaybe)

type Store = Map Name Value

initialStore :: Store
initialStore = Map.empty

extendedStore :: Store
extendedStore =
  Map.fromList
    [ ("x", IntVal 1),
      ("y", StringVal "mystring"),
      ("z", BoolVal True)
    ]

-- | Get the value for a Env name
envGet :: Name -> State Store (Maybe Value)
envGet n = do
  table <- S.get
  case Map.lookup n table of
    Just x -> return $ Just x
    Nothing -> return Nothing

-- | For some name update the value in the Env table
-- | If name doesn't exist, insert new
envUpdate :: Name -> Value -> State Store ()
envUpdate n v = do
  table <- S.get
  S.put (Map.insert n v table)

test_env :: Test
test_env =
  "index tests"
    ~: TestList
      [ S.evalState (envGet "x") extendedStore ~?= Just (IntVal 1),
        S.evalState (envGet "y") extendedStore ~?= Just (StringVal "mystring"),
        S.evalState (envGet "z") extendedStore ~?= Just (BoolVal True),
        S.evalState (envGet "yeehaw") extendedStore ~?= Nothing,
        -- Add new value to table
        S.evalState (envUpdate "k" (IntVal 20) >> envGet "k") extendedStore ~?= Just (IntVal 20),
        -- Update exisiting value in env table
        S.evalState (envUpdate "z" (BoolVal False) >> envGet "z") extendedStore ~?= Just (BoolVal False),
        -- Consecutive update test
        S.evalState (envUpdate "x" (IntVal 100) >> envUpdate "x" (IntVal 200) >> envGet "x") extendedStore ~?= Just (IntVal 200)
      ]

-- >>> runTestTT test_env
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

-- | Expression evaluator
evalE :: Expression -> State Store Value
evalE (Var v) = do
  mr <- envGet v
  case mr of
    Just r -> return r
    Nothing -> error $ "Variable not found: " ++ show v
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 o e1) = evalOp1 o <$> evalE e1
evalE (Expr e) = evalE e
evalE (CommandExpression cmd argsarr) = do
  s <- S.get
  cmd' <- evalE cmd
  let args' = foldr (comb s) [] argsarr
  let returnString = Commands.execCmd cmd' args' -- return the value from runCommand
  -- return (StringVal returnString)
  return (StringVal "Returning stub comment for now")
  where
    comb :: Store -> Expression -> [Value] -> [Value]
    comb s e acc =
      -- e' <- S.evalState (evalE e) st
      let e' = evaluate e s
       in e' : acc

-- | Handle unary operations
evalOp1 :: Uop -> Value -> Value
evalOp1 Not v = BoolVal $ not (toBool v)
evalOp1 DashZLen (StringVal []) = BoolVal True
evalOp1 DashZLen (StringVal _) = BoolVal False
evalOp1 DashNLen (StringVal []) = BoolVal False
evalOp1 DashNLen (StringVal _) = BoolVal True
evalOp1 _ _ = undefined -- other operations are not defined, todo: throw error

-- | Handle binary operations
evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = error "Divide by zero error"
evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
evalOp2 Modulo (IntVal i1) (IntVal i2) = IntVal (i1 `mod` i2)
evalOp2 Eq (IntVal i1) (IntVal i2) = BoolVal (i1 == i2)
evalOp2 Neq (IntVal i1) (IntVal i2) = BoolVal (i1 /= i2)
evalOp2 Gt (IntVal i1) (IntVal i2) = BoolVal (i1 > i2)
evalOp2 Ge (IntVal i1) (IntVal i2) = BoolVal (i1 >= i2)
evalOp2 Lt (IntVal i1) (IntVal i2) = BoolVal (i1 < i2)
evalOp2 Le (IntVal i1) (IntVal i2) = BoolVal (i1 <= i2)
evalOp2 Concat (StringVal s1) (StringVal s2) = StringVal (s1 ++ s2)
evalOp2 DashO (BoolVal i1) (BoolVal i2) = BoolVal (i1 || i2)
evalOp2 DashA (BoolVal i1) (BoolVal i2) = BoolVal (i1 && i2)
evalOp2 _ _ _ = error "Unsupported operation"

-- | Determine whether a value should be interpreted as true or false when
-- used as a condition.
toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool (StringVal "") = False
toBool _ = True

test_evaluateUop :: Test
test_evaluateUop =
  "evaluate uop"
    ~: TestList
      [ evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashZLen (Val (StringVal ""))) initialStore ~?= BoolVal True,
        evaluate (Op1 DashZLen (Val (StringVal "txt"))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashNLen (Val (StringVal ""))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashNLen (Val (StringVal "txt"))) initialStore ~?= BoolVal True
      ]

-- >>> runTestTT test_evaluateUop
-- Counts {cases = 7, tried = 7, errors = 0, failures = 0}

-- prop_evalE_total :: Expression -> Store -> Bool
-- prop_evalE_total e s = case evaluate e s of
--   NilVal -> True
--   IntVal i -> i `seq` True
--   BoolVal b -> b `seq` True
--   StringVal s -> s `seq` True
--   TableVal n -> n `seq` True

eval :: Block -> State Store ()
eval (Block ss) = mapM_ evalS ss

-- | Statement evaluator
evalS :: Statement -> State Store ()
evalS (Assign (Name v) e) = do
  e' <- evalE e
  envUpdate v e'
evalS (If e sb) = do
  e' <- evalE e
  when (toBool e') $ eval sb
evalS (IfElse e sb1 sb2) = do
  e' <- evalE e
  if toBool e' then eval sb1 else eval sb2
evalS Continue = return () -- think
evalS Break = return () -- think
evalS s@(While e sb) = do
  e' <- evalE e
  if toBool e'
    then eval sb >> evalS s
    else return ()
evalS s@(Until e sb) = do
  eval sb
  e' <- evalE e
  if not (toBool e')
    then evalS s
    else return () -- stop once expression is true
evalS (For (Name v) arr sb) =
  case arr of
    [] -> return ()
    x : tl -> do
      prevTable <- S.get -- add loop var in state
      envUpdate v x
      eval sb
      S.put prevTable -- restore state
      evalS (For (Name v) tl sb)
evalS (CommandStatement cmd argsarr) = do
  cmd' <- evalE cmd
  s <- S.get
  let args' = foldr (comb s) [] argsarr
  let retStr = Commands.execCmd cmd' args'
  return ()
  where
    comb :: Store -> Expression -> [Value] -> [Value]
    comb s e acc =
      let e' = evaluate e s
       in e' : acc

exec :: Block -> Store -> Store
exec = S.execState . eval

-- -- >>> runTestTT test_exec
-- -- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- test_exec :: Test
-- test_exec = TestList [tExecTest, tExecFact, tExecAbs, tExecTimes, tExecTable, tExecBfs]

-- -- | Evaluate a single statement and return the rest of the block
-- step :: Block -> State Store Block
-- step (Block []) = pure $ Block []
-- step (Block (x : xs)) =
--   case x of
--     If x1 b1 b2 -> do
--       s <- evalE x1
--       if toBool s then return $ b1 <> Block xs else return $ b2 <> Block xs
--     w@(While x1 b) -> do
--       s <- evalE x1
--       if toBool s
--         then return $ b <> Block (x : xs)
--         else return $ Block xs
--     -- r@(Repeat b x1) -> do
--     --   return $ b <> Block [If x1 (Block xs) (Block (x : xs))]
--     _ -> do
--       evalS x
--       return $ Block xs

-- -- | Make sure that we can step every block in every store
-- prop_step_total :: Block -> Store -> Bool
-- prop_step_total b s = case S.runState (step b) s of
--   (b', s') -> True

-- -- | Evaluate this block for a specified number of steps
-- boundedStep :: Int -> Block -> State Store Block
-- boundedStep i b =
--   if i > 0
--     then do
--       b' <- step b
--       boundedStep (i - 1) b'
--     else pure b

-- -- | Evaluate this block for a specified nuimber of steps, using the specified store
-- steps :: Int -> Block -> Store -> (Block, Store)
-- steps n block = S.runState (boundedStep n block)

-- -- | Is this block completely evaluated?
-- final :: Block -> Bool
-- final (Block []) = True
-- final _ = False

-- -- | Evaluate this block to completion
-- execStep :: Block -> Store -> Store
-- execStep = undefined

-- prop_stepExec :: Block -> QC.Property
-- prop_stepExec b =
--   not (final b) QC.==> final b1 QC.==> m1 == m2
--   where
--     (b1, m1) = S.runState (boundedStep 100 b) initialStore
--     m2 = exec b initialStore

-- -- >>> runTestTT test_execStep

-- tExecStepTest :: Test
-- tExecStepTest =
--   "execStep wTest"
--     ~: execStep wTest initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)]
--         )
--       ]

-- tExecStepFact :: Test
-- tExecStepFact =
--   "execStep wFact"
--     ~: execStep wFact initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList [(StringVal "f", IntVal 120), (StringVal "n", IntVal 0), (StringVal "x", IntVal 1), (StringVal "z", IntVal 120)]
--         )
--       ]

-- tExecStepAbs :: Test
-- tExecStepAbs =
--   "execStep wAbs"
--     ~: execStep wAbs initialStore
--     ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 3)])]

-- tExecStepTimes :: Test
-- tExecStepTimes =
--   "execStep wTimes"
--     ~: execStep wTimes initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
--         )
--       ]

-- tExecStepTable :: Test
-- tExecStepTable =
--   "execStep wTable"
--     ~: execStep wTable initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList
--             [ (StringVal "a", TableVal "_t1"),
--               (StringVal "k", IntVal 20),
--               (StringVal "o1", IntVal 10),
--               (StringVal "o2", StringVal "great"),
--               (StringVal "o3", IntVal 11)
--             ]
--         ),
--         ("_t1", Map.fromList [(IntVal 20, StringVal "great"), (StringVal "x", IntVal 11)])
--       ]

-- tExecStepBfs :: Test
-- tExecStepBfs =
--   "execStep wBfs"
--     ~: TestList
--       [ global !? StringVal "found" ~?= Just (BoolVal True)
--       ]
--   where
--     ss = execStep wBfs initialStore
--     global = case ss !? globalTableName of
--       Just g -> g
--       Nothing -> Map.empty

-- test_execStep :: Test
-- test_execStep = TestList [tExecStepFact, tExecStepAbs, tExecStepTimes, tExecStepAbs, tExecStepTable, tExecStepBfs]

data Stepper = Stepper
  { filename :: Maybe String,
    block :: Block,
    store :: Store,
    history :: Maybe Stepper
  }

initialStepper :: Stepper
initialStepper =
  Stepper
    { filename = Nothing,
      block = mempty,
      store = initialStore,
      history = Nothing
    }

-- -- | Retreives a past stepper based on our number of steps
-- getPrevSteppers :: Int -> Stepper -> Stepper
-- getPrevSteppers 0 s = s
-- getPrevSteppers i s = case history s of
--   Just s' -> getPrevSteppers (i - 1) s'
--   Nothing -> s

-- -- | Step through given steps at the same time
-- getNextSteppers :: Int -> Stepper -> Stepper
-- getNextSteppers 0 s = s
-- getNextSteppers i s = let (blk, s') = steps 1 (block s) (store s) in getNextSteppers (i - 1) s {block = blk, store = s', history = Just s}

-- Step across our Lu file and evaluate statement by statement
stepper :: IO ()
stepper = go initialStepper
  where
    go :: Stepper -> IO ()
    go ss = do
      prompt ss
      putStr (fromMaybe "sh" (filename ss) ++ "> ")
      str <- getLine
      case List.uncons (words str) of
        -- load a file for stepping
        Just (":l", [fn]) -> do
          result <- P.parseLuFile fn
          case result of
            Right x -> go ss {filename = Just fn, block = x, store = initialStore, history = Just ss}
            Left y -> do
              putStrLn y
              go ss
        -- dump the store
        Just (":d", _) -> do
          putStrLn (show (store ss))
          go ss
        -- quit the stepper
        Just (":q", _) -> return ()
        -- run current block to completion
        Just (":r", _) ->
          let s' = exec (block ss) (store ss)
           in go ss {block = mempty, store = s', history = Just ss}
          
        _ -> undefined
    -- -- next statement
    -- Just (":n", strs) ->
    --   let numSteps :: Int
    --       numSteps = case readMaybe (concat strs) of
    --         Just x -> x
    --         Nothing -> 1
    --    in go (getNextSteppers numSteps ss)
    -- -- previous statement
    -- Just (":p", strs) -> do
    --   let numSteps :: Int
    --       numSteps = case readMaybe (concat strs) of
    --         Just x -> x
    --         Nothing -> 1
    --    in let newSS = getPrevSteppers numSteps ss
    --        in go newSS {block = block newSS, store = store newSS, history = Just newSS}
    -- -- evaluate an expression in the current state
    -- _ -> case P.parseLuExp str of
    --   Right exp -> do
    --     let v = evaluate exp (store ss)
    --     -- putStrLn (pretty v)
    --       putStrLn "pretty v"
    --     go ss
    --   Left _s -> do
    --     putStrLn "?"
    --     go ss
    prompt :: Stepper -> IO ()
    prompt Stepper {block = Block []} = return ()
    prompt Stepper {block = Block (s : _)} =
      -- putStr "--> " >> putStrLn (pretty s)
      putStr "--> " >> putStrLn (show s)

-- type StateT :: Type -> (Type -> Type) -> Type -> Type
-- newtype StateT s m a = MkStateT {runStateT :: s -> m (a, s)}

-- instance Monad m => Monad (StateT s m) where
--   return :: a -> StateT s m a
--   return x = MkStateT $ \s -> return (x, s)
--   (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
--   p >>= f = MkStateT $ \s -> do
--     (r, s') <- runStateT p s
--     runStateT (f r) s'

-- instance Monad m => Applicative (StateT s m) where
--   pure = return
--   (<*>) = ap

-- instance Monad m => Functor (StateT s m) where
--   fmap = liftM

-- instance Monad m => MonadState s (StateT s m) where
--   get :: StateT s m s
--   get = MkStateT getIt
--     where
--       getIt :: s -> m (s, s)

--       getIt s = return (s, s)

--   put :: s -> StateT s m ()
--   put s = MkStateT putIt
--     where
--       putIt :: s -> m ((), s)

--       -- _s1 is the old state. we want to throw it away and replace it
--       -- with the new state s
--       putIt _s1 = return ((), s)

-- -------------------------- all properties and tests in this module  -----------------------------

-- test_all :: IO Counts
-- test_all = runTestTT $ TestList [test_index, test_update, test_resolveVar, test_evaluateNot, test_evaluateLen, test_exec, test_execStep]

-- -- >>> runTestTT test_all

-- qc :: IO ()
-- qc = do
--   putStrLn "evalE_total"
--   quickCheckN 100 prop_evalE_total
--   putStrLn "step_total"
--   quickCheckN 100 prop_step_total
--   putStrLn "stepExec"
--   quickCheckN 100 prop_stepExec
