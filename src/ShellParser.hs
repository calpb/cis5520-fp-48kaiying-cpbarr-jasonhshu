module ShellParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.List (isInfixOf)
import Parser (Parser)
import Parser qualified as P
import ShellSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

-- Handle continue and break (not just in parsing)
-- Parse between backticks (expect a command and some arguments)
-- Strings with quotes inside?
-- Handle semicolons like newline

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

-- dollarP :: Parser Expression
-- dollarP = P.char '$' *> expP

-- expressionListP :: Parser [Expression]
-- expressionListP = many (dollarP <|> expP)

-- wsP (dollarP s) *> pure ()

-- >>> runTestTT test_stringSubP

constP :: String -> a -> Parser a
constP s x = stringP s *> pure x

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

parens :: Parser a -> Parser a
parens x = P.between (stringP "(") x (stringP ")")

braces :: Parser a -> Parser a
braces x = P.between (stringP "{") x (stringP "}")

doubleBrackets :: Parser a -> Parser a
doubleBrackets x = P.between (stringP "[[") x (stringP "]]")

doubleParens :: Parser a -> Parser a
doubleParens x = P.between (stringP "((") x (stringP "))")

doubleBraces :: Parser a -> Parser a
doubleBraces x = P.between (stringP "{{") x (stringP "}}")

backticks :: Parser a -> Parser a
backticks x = P.between (P.char '`') x (P.char '`')

-- >>> P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]"
-- Right [1,1,1]
brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

valueP :: Parser Value
valueP = intValP <|> boolValP <|> stringValP

-- >>> P.parse (many intValP) "1 2\n 3"
-- Right [IntVal 1,IntVal 2,IntVal 3]
intValP :: Parser Value
intValP = IntVal <$> wsP P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

escQuotes :: Parser a -> Parser a
escQuotes x = P.between (P.string "\"") x (P.string "\"")

-- String values cannot contain $ in this implementation -- $ marks sub
stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP (P.filter (not . isInfixOf "$") (escQuotes (many (P.satisfy (/= '\"')))))

-- >>> P.parse stringValP "\"$a \""
test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

stringNoSubP :: Parser Value
stringNoSubP =
  StringVal
    <$> wsP
      ( (:)
          <$> P.satisfy (\c -> c /= '$' && c /= '\"')
          <*> many (P.satisfy (\c -> c /= '$' && c /= '\"'))
      )

stringNoSubSpaceP :: Parser Value
stringNoSubSpaceP =
  StringVal
    <$> wsP
      ( P.filter
          (not . isInfixOf "$")
          ( (:)
              <$> P.satisfy (\c -> (not . Char.isSpace) c && c /= '\"')
              <*> many (P.satisfy (\c -> (not . Char.isSpace) c && c /= '\"'))
          )
      )

-- >>> P.parse stringNoSpaceP "echo 1 2"
-- Right "echo"

-- >>> P.parse (many stringNoSpaceP) "echo 1 2"
-- Right ["echo","1","2"]

stringSubP :: Parser Expression
stringSubP =
  StringSub
    <$> wsP
      ( escQuotes
          ( (:)
              <$> ((Var <$> (P.char '$' *> nameP)) <|> (Val <$> stringNoSubP))
              <*> many ((Var <$> (P.char '$' *> nameP)) <|> (Val <$> stringNoSubP))
          )
      )

-- >>> P.parse (many stringSubP) "\"$a is neq to $b \" \" $a is eq to $a.\""
-- Right [StringSub [Var "a",Val (StringVal " is neq to "),Var "b",Val (StringVal " ")],StringSub [Val (StringVal " "),Var "a",Val (StringVal " is eq to "),Var "a",Val (StringVal ".")]]

commandStringP :: Parser Expression
commandStringP = (Val <$> stringNoSubSpaceP) <|> (Var <$> (P.char '$' *> nameP)) <|> stringSubP

-- >>> P.parse commandStringP "command \"$a1 neq $b2.\" $c something"

-- TODO : fix this part
-- Figure out operation precedence
expP :: Parser Expression
expP = compP
  where
    compP = catP `P.chainl1` opAtLevel (level Gt)
    catP = sumP `P.chainl1` opAtLevel (level Concat)
    sumP = prodP `P.chainl1` opAtLevel (level Plus)
    prodP = uopexpP `P.chainl1` opAtLevel (level Times)
    uopexpP =
      baseP
        <|> Op1 <$> uopP <*> uopexpP
    baseP =
      wsP (Var <$> (P.char '$' *> nameP))
        <|> parens expP -- ()
        <|> backticks expP -- ``
        <|> brackets expP -- []
        <|> Val <$> valueP
        <|> stringSubP

varP :: Parser Var
varP = Name <$> nameP

-- | Parse an operator at a specified precedence level
opAtLevel :: Int -> Parser (Expression -> Expression -> Expression)
opAtLevel l = flip Op2 <$> P.filter (\x -> level x == l) bopP

reserved :: [String]
reserved =
  [ "break",
    "continue",
    "do",
    "done",
    "else",
    "false",
    "fi",
    "for",
    "if",
    "in",
    "return",
    "then",
    "true",
    "until",
    "while"
  ]

-- >>> P.parse (many nameP) "x sfds _ -VAR5!"
-- Right ["x"]

-- TODO: not ignore new line

-- Var assignment in the shell is strict, i.e. e=1 parses but e = 1 does not
nameP :: Parser Name
nameP =
  -- wsP
  --   (
  P.filter
    (`notElem` reserved)
    ( (:)
        <$> P.choice [P.upper, P.lower, P.char '_']
        <*> many (P.choice [P.upper, P.lower, P.digit, P.char '_'])
    )

-- )

-- >>> P.parse (many uopP) "- - #"
-- Right [Neg,Neg,Len]
uopP :: Parser Uop
uopP =
  wsP
    ( P.choice
        [ constP "!" Not,
          constP "-z" DashZLen,
          constP "-n" DashNLen
        ]
    )

-- >>> P.parse (many bopP) "+ >= .."
-- Right [Plus,Ge,Concat]
bopP :: Parser Bop
bopP =
  wsP
    ( P.choice
        [ constP "==" Eq,
          constP "-eq" Eq,
          constP ">=" Ge,
          constP "-ge" Ge,
          constP ">" Gt,
          constP "-gt" Gt,
          constP "<=" Le,
          constP "-le" Le,
          constP "<" Lt,
          constP "-lt" Lt,
          constP "!=" Neq,
          constP "-ne" Neq,
          constP "+=" Concat,
          constP "-o" DashO,
          constP "-a" DashA,
          constP "+" Plus,
          constP "-" Minus,
          constP "\\*" Times,
          constP "//" Divide,
          constP "%" Modulo
        ]
    )

statementP :: Parser Statement
statementP =
  wsP
    ( P.choice
        [ constP "continue" Continue,
          constP "break" Break,
          Assign
            <$> varP
            <*> ( stringP "="
                    *> ( expP
                           <|> backticks
                             ( CommandExpression
                                 <$> (Val <$> stringNoSubSpaceP)
                                 <*> many (expP <|> commandStringP) -- CHECK THIS
                             )
                       )
                ),
          If
            <$> (stringP "if" *> expP)
            <*> (stringP "then" *> blockP <* stringP "fi"),
          IfElse
            <$> (stringP "ifelse" *> expP)
            <*> (stringP "then" *> blockP)
            <*> (stringP "else" *> blockP <* stringP "fi"),
          While
            <$> (stringP "while" *> expP)
            <*> (stringP "do" *> blockP <* stringP "done"),
          For
            <$> (stringP "for" *> wsP varP)
            <*> (stringP "in" *> many stringValP)
            <*> (stringP "do" *> blockP <* stringP "done"),
          Until
            <$> (stringP "until" *> expP)
            <*> (stringP "do" *> blockP <* stringP "done"),
          CommandStatement
            <$> (Val <$> stringNoSubSpaceP)
            <*> many commandStringP
        ]
    )

blockP :: Parser Block
blockP = Block <$> many statementP

parseLuExp :: String -> Either P.ParseError Expression
parseLuExp = P.parse expP

parseLuStat :: String -> Either P.ParseError Statement
parseLuStat = P.parse statementP

parseLuFile :: String -> IO (Either P.ParseError Block)
parseLuFile = P.parseFromFile (const <$> blockP <*> P.eof)

tParseFiles :: Test
tParseFiles =
  "parse files"
    ~: TestList
      []
  where
    -- "fact" ~: p "lu/fact.lu" wFact,
    -- TODO fix me

    p fn ast = do
      result <- parseLuFile fn
      case result of
        (Left _) -> assert False
        (Right ast') -> assert (ast == ast')

-- test_comb =
--   "parsing combinators"
--     ~: TestList
--       [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
--         P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
--         P.parse (stringP "a") "a" ~?= Right (),
--         P.parse (stringP "a") "b" ~?= Left "No parses",
--         P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
--         P.parse (constP "&" 'a') "&  " ~?= Right 'a',
--         P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
--         P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
--       ]

test_value =
  "parsing values"
    ~: TestList
      [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
        P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
        P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
        P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
        P.parse stringValP "\"abcd ef\"" ~?= Right (StringVal "abcd ef"),
        P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
        P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"],
        P.parse stringNoSubSpaceP "cd" ~?= Right (StringVal "cd"),
        P.parse (many commandStringP) "cd /bin/ls" ~?= Right [Val (StringVal "cd"), Val (StringVal "/bin/ls")]
      ]

-- >>> runTestTT test_value
-- Counts {cases = 9, tried = 9, errors = 0, failures = 0}

-- >>> runTestTT test_exp
-- Counts {cases = 10, tried = 10, errors = 0, failures = 0}
-- >>> P.parse (many expP) "`$a+5`"
-- Right [Op2 (Var "a") Plus (Val (IntVal 5))]

test_exp =
  "parsing expressions"
    ~: TestList
      [ P.parse (many varP) "x y z TEST" ~?= Right [Name "x"],
        P.parse (many nameP) "TEST " ~?= Right ["TEST"],
        P.parse (many nameP) "__ " ~?= Right ["__"],
        P.parse (many nameP) "eee " ~?= Right ["eee"],
        P.parse (many uopP) "! -z -n" ~?= Right [Not, DashZLen, DashNLen],
        P.parse (many bopP) "+   -   \\*   //  %" ~?= Right [Plus, Minus, Times, Divide, Modulo],
        P.parse (many bopP) "== -eq != -ne > -gt >= -ge < -lt <= -le" ~?= Right [Eq, Eq, Neq, Neq, Gt, Gt, Ge, Ge, Lt, Lt, Le, Le],
        P.parse (many bopP) "-o -a" ~?= Right [DashO, DashA],
        P.parse (many bopP) "+= += " ~?= Right [Concat, Concat],
        P.parse (many expP) "`$a+5`" ~?= Right [Op2 (Var "a") Plus (Val (IntVal 5))]
        -- ,
        -- P.parse () "echo helloworld"
        --   ~?= Right [CommandExpression (StringVal) ([Expression])]
      ]

test_stat =
  "parsing statements"
    ~: TestList
      [ P.parse statementP "VAR=3" ~?= Right (Assign (Name "VAR") (Val (IntVal 3))),
        P.parse statementP "echo \"hello $a\""
          ~?= Right
            ( CommandStatement
                (Val $ StringVal "echo")
                [ StringSub
                    [Val (StringVal "hello "), Var "a"]
                ]
            )
            --   P.parse statementP "if x then y=nil else end"
            --     ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
            --   P.parse statementP "while nil do end"
            --     ~?= Right (While (Val NilVal) (Block [])),
            --   P.parse statementP "repeat ; ; until false"
            --     ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
      ]

-- >>> runTestTT test_stat
-- Counts {cases = 2, tried = 2, errors = 0, failures = 0}

test_all :: IO Counts
test_all = runTestTT $ TestList [test_value, test_exp, test_stat, tParseFiles] -- test_comb

qc :: IO ()
qc = do
  putStrLn "roundtrip_val"
  -- QC.quickCheck prop_roundtrip_val
  putStrLn "roundtrip_exp"
  -- QC.quickCheck prop_roundtrip_exp
  putStrLn "roundtrip_stat"

-- QC.quickCheck prop_roundtrip_stat
