module ShellInterpreter where

import Control.Monad (unless, when)
import Data.List qualified as List
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import ShellParser 
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
  undefined

-- update :: Reference -> Value -> State Store ()
-- update (_, NilVal) _ = return ()
-- update (n, k) v' =
--   S.get >>= \s -> case Map.lookup n s of
--     Just t ->
--       let t' =
--             if v' == NilVal
--               then Map.delete k t
--               else Map.insert k v' t
--        in S.put (Map.insert n t' s)
--     Nothing -> return ()

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

-- -- >>> runTestTT test_update
-- -- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- | Expression evaluator
evalE :: Expression -> State Store Value
evalE (Var v) = do
  mr <- envGet v -- see above
  case mr of
    Just r -> return r
    Nothing -> return $ StringVal "yooooooo thats not allowed " -- todo: THROW ERROR FOUND UNDEFINED VARIABLE, could use either monad
evalE (Val v) = return v
evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
evalE (Op1 _o _e1) = undefined
    -- S.get >>= (\s -> evalOp1 s _o <$> evalE _e1)
evalE (Expr e) = undefined

-- | Handle unary operations
evalOp1 :: Uop -> Value -> Value
evalOp1 Not v = BoolVal $ not (toBool v)
evalOp1 DashZLen v = undefined
evalOp1 DashNLen v = undefined
evalOp1 Str v = undefined

-- | Handle binary operations
evalOp2 :: Bop -> Value -> Value -> Value
evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
evalOp2 Divide (IntVal _) (IntVal 0) = undefined -- throw error
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
evalOp2 _ _ _ = undefined -- throw error

evaluate :: Expression -> Store -> Value
evaluate e = S.evalState (evalE e)

-- | Determine whether a value should be interpreted as true or false when
-- used as a condition.
toBool :: Value -> Bool
toBool (BoolVal False) = False
toBool _ = True

test_evaluateUop :: Test
test_evaluateUop =
  "evaluate uop"
    ~: TestList
      [ evaluate (Op1 Not (Val NilVal)) initialStore ~?= BoolVal True,
        evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashZLen (Val (BoolVal False))) initialStore ~?= BoolVal True,
        evaluate (Op1 DashZLen (Val (BoolVal True))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashNLen (Val (StringVal ""))) initialStore ~?= BoolVal False,
        evaluate (Op1 DashNLen (Val (TableVal "_G"))) initialStore ~?= BoolVal False,
        evaluate (Op1 Str (Val (StringVal "_G"))) initialStore ~?= BoolVal False,
        evaluate (Op1 Str (Val (StringVal ""))) initialStore ~?= BoolVal True
      ]

-- -- >>> runTestTT test_evaluateNot
-- -- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- test_evaluateLen :: Test
-- test_evaluateLen =
--   "evaluate len"
--     ~: TestList
--       [ evaluate (Op1 Len (Val (StringVal "552"))) extendedStore ~?= IntVal 3,
--         evaluate (Op1 Len (Val (TableVal "_G"))) extendedStore ~?= IntVal 2,
--         evaluate (Op1 Len (Val (TableVal "_t1"))) extendedStore ~?= IntVal 2,
--         evaluate (Op1 Len (Val NilVal)) initialStore ~?= NilVal,
--         evaluate (Op1 Len (Val (IntVal 3))) initialStore ~?= IntVal 3,
--         evaluate (Op1 Len (Val (BoolVal False))) initialStore ~?= IntVal 0,
--         evaluate (Op1 Len (Val (BoolVal True))) initialStore ~?= IntVal 1,
--         evaluate (Op1 Len (Val (StringVal ""))) initialStore ~?= IntVal 0,
--         evaluate (Op1 Len (Val (TableVal "_t1"))) initialStore ~?= NilVal
--       ]

-- -- >>> runTestTT test_evaluateLen
-- -- Counts {cases = 9, tried = 9, errors = 0, failures = 0}

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
evalS (If e s1 s2) = do
  v <- evalE e
  if toBool v then eval s1 else eval s2
evalS w@(While e ss) = do
  v <- evalE e
  when (toBool v) $ do
    eval ss
    evalS w
evalS (Assign _v _e) = do
  -- update global variable or table field v to value of e
  v <- evalE _e
  ref <- resolveVar _v
  case ref of
    Just ref' -> update ref' v
    Nothing -> return ()
evalS w@(Until _b _e) = do
  -- keep evaluating block b until expression e is true
  eval _b
  v <- evalE _e
  unless (toBool v) $ do
    evalS w
evalS w@(Command s e) = do
  -- e = [Val, Val, Var, Val] -> [Vals] -> system.process.shell
  -- call substituteExpr on e
  -- 1 = echo
  -- 1 "text here"
  undefined

substituteExpr :: [Expression] -> [Expression]
substituteExpr [] = []
substituteExpr ((Var x) : xs) = evalE x : substituteExpr xs
substituteExpr ((Val x) : xs) = x : substituteExpr xs

exec :: Block -> Store -> Store
exec = S.execState . eval

-- -- >>> runTestTT test_exec
-- -- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- test_exec :: Test
-- test_exec = TestList [tExecTest, tExecFact, tExecAbs, tExecTimes, tExecTable, tExecBfs]

-- step :: Block -> State Store Block
-- step _ = undefined

-- -- | Make sure that we can step every block in every store
-- prop_step_total :: Block -> Store -> Bool
-- prop_step_total b s = case S.runState (step b) s of
--   (b', s') -> True

-- -- | Evaluate this block for a specified number of steps
-- boundedStep :: Int -> Block -> State Store Block
-- boundedStep = undefined

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

-- data Stepper = Stepper
--   { filename :: Maybe String,
--     block :: Block,
--     store :: Store,
--     history :: Maybe Stepper
--   }

-- initialStepper :: Stepper
-- initialStepper =
--   Stepper
--     { filename = Nothing,
--       block = mempty,
--       store = initialStore,
--       history = Nothing
--     }

-- -- Fill in `undefined` below
-- stepper :: IO ()
-- stepper = go initialStepper
--   where
--     go :: Stepper -> IO ()
--     go ss = do
--       prompt ss
--       putStr (fromMaybe "Lu" (filename ss) ++ "> ")
--       str <- getLine
--       case List.uncons (words str) of
--         -- load a file for stepping
--         Just (":l", [fn]) -> do
--           undefined
--         -- dump the store
--         Just (":d", _) -> do
--           putStrLn (pretty (store ss))
--           go ss
--         -- quit the stepper
--         Just (":q", _) -> return ()
--         -- run current block to completion
--         Just (":r", _) ->
--           let s' = exec (block ss) (store ss)
--            in go ss {block = mempty, store = s', history = Just ss}
--         -- next statement
--         Just (":n", strs) -> do
--           let numSteps :: Int
--               numSteps = case readMaybe (concat strs) of
--                 Just x -> x
--                 Nothing -> 1
--           undefined
--         -- previous statement
--         Just (":p", strs) -> do
--           let numSteps :: Int
--               numSteps = case readMaybe (concat strs) of
--                 Just x -> x
--                 Nothing -> 1
--           undefined
--         -- evaluate an expression in the current state
--         _ -> case LuParser.parseLuExp str of
--           Right exp -> do
--             let v = evaluate exp (store ss)
--             putStrLn (pretty v)
--             go ss
--           Left _s -> do
--             putStrLn "?"
--             go ss
--     prompt :: Stepper -> IO ()
--     prompt Stepper {block = Block []} = return ()
--     prompt Stepper {block = Block (s : _)} =
--       putStr "--> " >> putStrLn (pretty s)

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