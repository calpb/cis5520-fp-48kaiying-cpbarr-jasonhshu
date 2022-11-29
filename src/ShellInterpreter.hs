-- module LuStepper where

-- import Control.Monad (unless, when)
-- import Data.List qualified as List
-- import Data.Map (Map, (!?))
-- import Data.Map qualified as Map
-- import Data.Maybe (fromMaybe)
-- import ShellParser qualified
-- import ShellSyntax
-- import State (State)
-- import State qualified as S
-- import Test.HUnit (Counts, Test (..), runTestTT, (~:), (~?=))
-- import Test.QuickCheck qualified as QC
-- import Text.Read (readMaybe)

-- type Store = Map Name Table

-- type Table = Map Value Value

-- globalTableName :: Name
-- globalTableName = "_G"

-- initialStore :: Store
-- initialStore = Map.singleton globalTableName Map.empty

-- extendedStore :: Store
-- extendedStore =
--   Map.fromList
--     [ ( globalTableName,
--         Map.fromList
--           [ (StringVal "x", IntVal 3),
--             (StringVal "t", TableVal "_t1")
--           ]
--       ),
--       ( "_t1",
--         Map.fromList
--           [ (StringVal "y", BoolVal True),
--             (IntVal 2, TableVal "_t1")
--           ]
--       )
--     ]

-- -- >>> oneLine initialStore
-- -- "{_G = {}}"

-- -- >>> oneLine extendedStore
-- -- "{_G = {t = <_t1> x = 3} _t1 = {[2] = <_t1> y = true}}"

-- type Reference = (Name, Value)

-- xref :: Reference
-- xref = ("_G", StringVal "x")

-- yref :: Reference
-- yref = ("_t1", StringVal "y")

-- index :: Reference -> State Store Value
-- index (_, NilVal) = return NilVal
-- index (n, k) =
--   S.get >>= \s -> case Map.lookup n s of
--     Just t -> case Map.lookup k t of
--       Just v -> return v
--       Nothing -> return NilVal
--     Nothing -> return NilVal

-- test_index :: Test
-- test_index =
--   "index tests"
--     ~: TestList
--       [ -- The global variable "x" is unitialized (i.e. not present in the initial store)
--         S.evalState (index xref) initialStore ~?= NilVal,
--         -- But there is a value for "x" available in the extended store
--         S.evalState (index xref) extendedStore ~?= IntVal 3,
--         -- If a table is not found in the store, accessing its reference also returns nil.
--         S.evalState (index yref) initialStore ~?= NilVal,
--         -- We should also be able to access "t[1]" in the extended store
--         S.evalState (index yref) extendedStore ~?= BoolVal True,
--         -- Updates using the `nil` key are ignored
--         S.execState (update ("_t1", NilVal) (IntVal 3)) extendedStore ~?= extendedStore
--       ]

-- -- >>> runTestTT test_index
-- -- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

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

-- test_update :: Test
-- test_update =
--   "index tests"
--     ~: TestList
--       [ -- If we assign to x, then we can find its new value
--         S.evalState (update xref (IntVal 4) >> index xref) initialStore ~?= IntVal 4,
--         -- If we assign to x, then remove it, we cannot find it anymore
--         S.evalState (update xref (IntVal 4) >> update xref NilVal >> index xref) initialStore ~?= NilVal,
--         -- If we assign to t.y, then we can find its new value
--         S.evalState (update yref (IntVal 5) >> index yref) extendedStore ~?= IntVal 5,
--         -- If we assign nil to t.y, then we cannot find it
--         S.evalState (update yref NilVal >> index yref) extendedStore ~?= NilVal
--       ]

-- -- >>> runTestTT test_update
-- -- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

-- allocateTable :: [(Value, Value)] -> State Store Value
-- allocateTable assocs = do
--   store <- S.get
--   -- make a fresh name for the new table
--   let n = length (Map.keys store)
--   let tableName = "_t" ++ show n
--   -- make sure we don't have a nil key or value
--   let assocs' = filter nonNil assocs
--   -- update the store
--   S.put (Map.insert tableName (Map.fromList assocs') store)
--   return (TableVal tableName)

-- -- Keep nil out of the table
-- nonNil :: (Value, Value) -> Bool
-- nonNil (k, v) = k /= NilVal && v /= NilVal

-- -- | Convert a variable into a reference into the store.
-- -- Fails when the var is `t.x` or t[1] and `t` is not defined in the store
-- -- when the var is `2.y` or `nil[2]` (i.e. not a `TableVal`)
-- -- or when the var is t[nil]
-- resolveVar :: Var -> State Store (Maybe Reference)
-- resolveVar (Name k) = return $ return (globalTableName, StringVal k)
-- resolveVar (Dot exp k) = do
--   s <- S.get
--   case S.evalState (evalE exp) s of
--     TableVal tv -> return $ return (tv, StringVal k)
--     _ -> return Nothing
-- resolveVar (Proj exp1 exp2) = do
--   s <- S.get
--   case S.evalState (evalE exp1) s of
--     TableVal tv -> case S.evalState (evalE exp2) s of
--       NilVal -> return Nothing
--       val -> return $ return (tv, val)
--     _ -> return Nothing

-- test_resolveVar :: Test
-- test_resolveVar =
--   "resolveVar"
--     ~: TestList
--       [ -- we should be able to resolve global variable `x` in the initial store, even though it is not defined
--         S.evalState (resolveVar (Name "x")) initialStore ~?= Just ("_G", StringVal "x"),
--         -- But in the case of Dot or Proj, the first argument should evaluate to a
--         -- TableVal that is defined in the store. If it does not, then resolveVar
--         -- should return Nothing.
--         S.evalState (resolveVar (Dot (Val NilVal) "x")) initialStore ~?= Nothing,
--         S.evalState (resolveVar (Dot (Var (Name "t")) "x")) initialStore ~?= Nothing,
--         -- For Proj we also shouldn't project from Nil
--         S.evalState (resolveVar (Proj (Var (Name "t")) (Val NilVal))) extendedStore ~?= Nothing,
--         -- If the table is defined, we should return a reference to it, even when the field is undefined
--         S.evalState (resolveVar (Dot (Var (Name "t")) "z")) extendedStore ~?= Just ("_t1", StringVal "z"),
--         -- and how we refer to the field shouldn't matter
--         S.evalState (resolveVar (Proj (Var (Name "t")) (Val (StringVal "z")))) extendedStore ~?= Just ("_t1", StringVal "z")
--       ]

-- -- >>> runTestTT test_resolveVar
-- -- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- -- | Expression evaluator
-- evalE :: Expression -> State Store Value
-- evalE (Var v) = do
--   mr <- resolveVar v -- see above
--   case mr of
--     Just r -> index r
--     Nothing -> return NilVal
-- evalE (Val v) = return v
-- evalE (Op2 e1 o e2) = evalOp2 o <$> evalE e1 <*> evalE e2
-- evalE (Op1 _o _e1) = S.get >>= (\s -> evalOp1 s _o <$> evalE _e1)
-- evalE (TableConst _fs) = resolveTableField _fs >>= allocateTable

-- -- | Construct a new table by transforming table fields into list of associations
-- resolveTableField :: [TableField] -> State Store [(Value, Value)]
-- resolveTableField [] = return []
-- resolveTableField (FieldName n exp : xs) = (:) <$> ((StringVal n,) <$> evalE exp) <*> resolveTableField xs
-- resolveTableField ((FieldKey exp1 exp2) : xs) = (:) <$> ((,) <$> evalE exp1 <*> evalE exp2) <*> resolveTableField xs

-- -- | Handle unary operations
-- evalOp1 :: Store -> Uop -> Value -> Value
-- evalOp1 _ Neg (IntVal i) = IntVal $ -i
-- evalOp1 _ Not v = BoolVal $ not (toBool v)
-- evalOp1 _ Len (StringVal s) = IntVal $ length s
-- evalOp1 s Len (TableVal t) = case Map.lookup t s of
--   Just t' -> IntVal $ Map.size t'
--   Nothing -> NilVal
-- evalOp1 _ Len (IntVal i) = IntVal i
-- evalOp1 _ Len (BoolVal True) = IntVal 1
-- evalOp1 _ Len (BoolVal False) = IntVal 0
-- evalOp1 _ _ _ = NilVal

-- -- | Handle binary operations
-- evalOp2 :: Bop -> Value -> Value -> Value
-- evalOp2 Plus (IntVal i1) (IntVal i2) = IntVal (i1 + i2)
-- evalOp2 Minus (IntVal i1) (IntVal i2) = IntVal (i1 - i2)
-- evalOp2 Times (IntVal i1) (IntVal i2) = IntVal (i1 * i2)
-- evalOp2 Divide (IntVal _) (IntVal 0) = NilVal
-- evalOp2 Divide (IntVal i1) (IntVal i2) = IntVal (i1 `div` i2)
-- evalOp2 Modulo (IntVal i1) (IntVal 0) = NilVal
-- evalOp2 Modulo (IntVal i1) (IntVal i2) = IntVal (i1 `mod` i2)
-- evalOp2 Eq NilVal NilVal = BoolVal True
-- evalOp2 Eq (IntVal i1) (IntVal i2) = BoolVal $ i1 == i2
-- evalOp2 Eq (BoolVal b1) (BoolVal b2) = BoolVal $ b1 == b2
-- evalOp2 Eq (StringVal s1) (StringVal s2) = BoolVal $ s1 == s2
-- evalOp2 Eq (TableVal t1) (TableVal t2) = BoolVal $ t1 == t2
-- evalOp2 Eq _ _ = BoolVal False
-- evalOp2 _ (TableVal t1) (TableVal t2) = BoolVal False -- unspecified order
-- evalOp2 Gt (TableVal t1) _ = BoolVal True
-- evalOp2 Gt (StringVal s1) (StringVal s2) = BoolVal $ s1 > s2
-- evalOp2 Gt (StringVal s1) (TableVal _) = BoolVal False
-- evalOp2 Gt (StringVal s1) _ = BoolVal True
-- evalOp2 Gt (BoolVal b1) (BoolVal b2) = BoolVal $ b1 && not b2
-- evalOp2 Gt (BoolVal _) NilVal = BoolVal True
-- evalOp2 Gt (BoolVal _) (IntVal _) = BoolVal True
-- evalOp2 Gt (BoolVal _) (StringVal _) = BoolVal False
-- evalOp2 Gt (BoolVal _) (TableVal _) = BoolVal False
-- evalOp2 Gt (IntVal i1) (IntVal i2) = BoolVal $ i1 > i2
-- evalOp2 Gt (IntVal i1) NilVal = BoolVal True
-- evalOp2 Gt (IntVal i1) _ = BoolVal False
-- evalOp2 Gt NilVal _ = BoolVal False
-- evalOp2 Ge v1 v2 = case (evalOp2 Gt v1 v2, evalOp2 Eq v1 v2) of
--   (BoolVal b1, BoolVal b2) -> BoolVal $ b1 || b2
--   (_, _) -> error "nonexistent"
-- evalOp2 Lt v1 v2 = case evalOp2 Ge v1 v2 of
--   BoolVal b -> BoolVal $ not b
--   _ -> error "nonexistent"
-- evalOp2 Le v1 v2 = case evalOp2 Gt v1 v2 of
--   BoolVal b -> BoolVal $ not b
--   _ -> error "nonexistent"
-- evalOp2 Concat (StringVal s1) (StringVal s2) = StringVal $ s1 ++ s2
-- evalOp2 _ _ _ = NilVal

-- evaluate :: Expression -> Store -> Value
-- evaluate e = S.evalState (evalE e)

-- -- | Determine whether a value should be interpreted as true or false when
-- -- used as a condition.
-- toBool :: Value -> Bool
-- toBool (BoolVal False) = False
-- toBool NilVal = False
-- toBool _ = True

-- test_evaluateNot :: Test
-- test_evaluateNot =
--   "evaluate not"
--     ~: TestList
--       [ evaluate (Op1 Not (Val NilVal)) initialStore ~?= BoolVal True,
--         evaluate (Op1 Not (Val (IntVal 3))) initialStore ~?= BoolVal False,
--         evaluate (Op1 Not (Val (BoolVal False))) initialStore ~?= BoolVal True,
--         evaluate (Op1 Not (Val (BoolVal True))) initialStore ~?= BoolVal False,
--         evaluate (Op1 Not (Val (StringVal ""))) initialStore ~?= BoolVal False,
--         evaluate (Op1 Not (Val (TableVal "_G"))) initialStore ~?= BoolVal False
--       ]

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

-- test_tableConst :: Test
-- test_tableConst =
--   "evaluate table constructors "
--     ~: TestList
--       [ S.runState
--           (evalE (TableConst [FieldName "x" (Val (IntVal 3))]))
--           initialStore
--           ~?= ( TableVal "_t1",
--                 Map.fromList
--                   [ ("_G", Map.empty),
--                     ("_t1", Map.fromList [(StringVal "x", IntVal 3)])
--                   ]
--               ),
--         S.runState
--           (evalE (TableConst [FieldName "x" (Val (IntVal 3)), FieldKey (Val (BoolVal True)) (Val (BoolVal True))]))
--           initialStore
--           ~?= ( TableVal "_t1",
--                 Map.fromList
--                   [ ("_G", Map.empty),
--                     ("_t1", Map.fromList [(StringVal "x", IntVal 3), (BoolVal True, BoolVal True)])
--                   ]
--               ),
--         S.runState
--           (evalE (TableConst []))
--           extendedStore
--           ~?= ( TableVal "_t2",
--                 Map.fromList
--                   [ ( globalTableName,
--                       Map.fromList
--                         [ (StringVal "x", IntVal 3),
--                           (StringVal "t", TableVal "_t1")
--                         ]
--                     ),
--                     ( "_t1",
--                       Map.fromList
--                         [ (StringVal "y", BoolVal True),
--                           (IntVal 2, TableVal "_t1")
--                         ]
--                     ),
--                     ("_t2", Map.empty)
--                   ]
--               )
--       ]

-- -- >>> runTestTT test_tableConst
-- -- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-- prop_evalE_total :: Expression -> Store -> Bool
-- prop_evalE_total e s = case evaluate e s of
--   NilVal -> True
--   IntVal i -> i `seq` True
--   BoolVal b -> b `seq` True
--   StringVal s -> s `seq` True
--   TableVal n -> n `seq` True

-- eval :: Block -> State Store ()
-- eval (Block ss) = mapM_ evalS ss

-- -- | Statement evaluator
-- evalS :: Statement -> State Store ()
-- evalS (If e s1 s2) = do
--   v <- evalE e
--   if toBool v then eval s1 else eval s2
-- evalS w@(While e ss) = do
--   v <- evalE e
--   when (toBool v) $ do
--     eval ss
--     evalS w
-- evalS (Assign _v _e) = do
--   -- update global variable or table field v to value of e
--   v <- evalE _e
--   ref <- resolveVar _v
--   case ref of
--     Just ref' -> update ref' v
--     Nothing -> return ()
-- evalS w@(Repeat _b _e) = do
--   -- keep evaluating block b until expression e is true
--   eval _b
--   v <- evalE _e
--   unless (toBool v) $ do
--     evalS w
-- evalS Empty = return () -- do nothing

-- exec :: Block -> Store -> Store
-- exec = S.execState . eval

-- -- >>> runTestTT test_exec
-- -- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- -------------------------- Test cases for exec -----------------------------

-- tExecTest :: Test
-- tExecTest =
--   "exec wTest"
--     ~: exec wTest initialStore
--     ~?= Map.fromList [(globalTableName, Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 10)])]

-- tExecFact :: Test
-- tExecFact =
--   "exec wFact"
--     ~: exec wFact initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList
--             [ (StringVal "f", IntVal 120),
--               (StringVal "n", IntVal 0),
--               (StringVal "x", IntVal 1),
--               (StringVal "z", IntVal 120)
--             ]
--         )
--       ]

-- tExecAbs :: Test
-- tExecAbs =
--   "exec wAbs"
--     ~: exec wAbs initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList [(StringVal "x", IntVal 3)]
--         )
--       ]

-- tExecTimes :: Test
-- tExecTimes =
--   "exec wTimes"
--     ~: exec wTimes initialStore
--     ~?= Map.fromList
--       [ ( globalTableName,
--           Map.fromList [(StringVal "x", IntVal 0), (StringVal "y", IntVal 3), (StringVal "z", IntVal 30)]
--         )
--       ]

-- tExecTable :: Test
-- tExecTable =
--   "exec wTable"
--     ~: exec wTable initialStore
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

-- tExecBfs :: Test
-- tExecBfs = "exec wBfs" ~: TestList [global !? StringVal "found" ~?= Just (BoolVal True)]
--   where
--     ss = exec wBfs initialStore
--     global = case ss !? globalTableName of
--       Just g -> g
--       Nothing -> Map.empty

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