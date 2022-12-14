module ShellParser where

import Control.Applicative
import Data.Char qualified as Char
import Data.List (isInfixOf)
import Parser (Parser)
import Parser qualified as P
import ShellSyntax
import Test.HUnit (Assertion, Counts, Test (..), assert, runTestTT, (~:), (~?=))
import Test.QuickCheck qualified as QC

wsP :: Parser a -> Parser a
wsP p = p <* many P.space

-- wsP not including newline
wsP' :: Parser a -> Parser a
wsP' p = p <* many (P.satisfy (\c -> Char.isSpace c && c /= '\n'))

test_wsP :: Test
test_wsP =
  TestList
    [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
      P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
      P.parse (many (wsP' P.alpha)) "a b \n   \t c" ~?= Right "ab"
    ]

-- >>> runTestTT test_wsP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

stringP :: String -> Parser ()
stringP s = wsP (P.string s) *> pure ()

stringP' :: String -> Parser ()
stringP' s = wsP' (P.string s) *> pure ()

test_stringP :: Test
test_stringP =
  TestList
    [ P.parse (stringP "a") "a" ~?= Right (),
      P.parse (stringP "a") "b" ~?= Left "No parses",
      P.parse (many (stringP "a")) "a\n  a" ~?= Right [(), ()],
      P.parse (many (stringP' "a")) "a\n  a" ~?= Right [()]
    ]

-- >>> runTestTT test_stringP
-- Counts {cases = 4, tried = 4, errors = 0, failures = 0}

constP :: String -> a -> Parser a
constP s x = stringP s *> pure x

constP' :: String -> a -> Parser a
constP' s x = stringP' s *> pure x

test_constP :: Test
test_constP =
  TestList
    [ P.parse (constP "&" 'a') "&  " ~?= Right 'a',
      P.parse (many (constP "&" 'a')) "&\n   &" ~?= Right "aa",
      P.parse (many (constP' "&" 'a')) "&\n   &" ~?= Right "a"
    ]

-- >>> runTestTT test_constP
-- Counts {cases = 3, tried = 3, errors = 0, failures = 0}

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

brackets :: Parser a -> Parser a
brackets x = P.between (stringP "[") x (stringP "]")

valueP :: Parser Value
valueP = intValP <|> boolValP <|> stringValP

valueP' :: Parser Value
valueP' = intValP' <|> boolValP' <|> stringValP'

intValP :: Parser Value
intValP = IntVal <$> wsP P.int

intValP' :: Parser Value
intValP' = IntVal <$> wsP' P.int

-- >>> P.parse (many boolValP) "true false\n true"
-- Right [BoolVal True,BoolVal False,BoolVal True]
boolValP :: Parser Value
boolValP = constP "true" (BoolVal True) <|> constP "false" (BoolVal False)

boolValP' :: Parser Value
boolValP' = constP' "true" (BoolVal True) <|> constP' "false" (BoolVal False)

escQuotes :: Parser a -> Parser a
escQuotes x = P.between (P.string "\"") x (P.string "\"")

-- String values cannot contain $ in this implementation -- $ marks sub
stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP (P.filter (not . isInfixOf "$") (escQuotes (many (P.satisfy (\c -> c /= '\n' && c /= '\"')))))

-- >>> P.parse stringValP "\"$a \""

stringValP' :: Parser Value
stringValP' =
  StringVal
    <$> wsP' (P.filter (not . isInfixOf "$") (escQuotes (many (P.satisfy (\c -> c /= '\n' && c /= '\"')))))

test_stringValP :: Test
test_stringValP =
  TestList
    [ P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
      P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
      P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"],
      P.parse (many stringValP) "\" a\"   \"$b\"" ~?= Right [StringVal " a"],
      P.parse (many stringValP) "\" $a\"   \"$b\"" ~?= Right []
    ]

-- >>> runTestTT test_stringValP
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

-- Parse string until you hit $, \", or `
stringStopSubP :: Parser Value
stringStopSubP =
  let reserved_chars = ['$', '\"', '`']
   in StringVal
        <$> wsP
          ( (:)
              <$> P.satisfy (`notElem` reserved_chars)
              <*> many (P.satisfy (`notElem` reserved_chars))
          )

-- | Only accept strings that don't contain a dollar sign and stop at a space, esc quotes, backtick
stringNoSubSpaceP :: Parser Value
stringNoSubSpaceP =
  let reserved_chars = ['\"', '`']
   in StringVal
        <$> wsP'
          ( P.filter
              (not . isInfixOf "$")
              ( (:)
                  <$> P.satisfy (\c -> (not . Char.isSpace) c && notElem c reserved_chars)
                  <*> many (P.satisfy (\c -> (not . Char.isSpace) c && notElem c reserved_chars))
              )
          )

-- A generalization of stringValP which allows for substitution (parses variable names)
stringSubP :: Parser Expression
stringSubP =
  StringSub
    <$> wsP'
      ( escQuotes
          ( (:)
              <$> ((Var <$> (P.char '$' *> nameP)) <|> (Val <$> stringStopSubP))
              <*> many ((Var <$> (P.char '$' *> nameP)) <|> (Val <$> stringStopSubP))
          )
      )

test_stringSub :: Test
test_stringSub =
  TestList
    [ P.parse (many stringStopSubP) " $a" ~?= Right [StringVal " "],
      P.parse stringStopSubP "\"a\\\"\"" ~?= Left "No parses",
      P.parse stringStopSubP "The value is $a" ~?= Right (StringVal "The value is "),
      P.parse stringNoSubSpaceP "echo 1 2" ~?= Right (StringVal "echo"),
      P.parse stringSubP "\"$a is neq to $b \" " ~?= Right (StringSub [Var "a", Val (StringVal " is neq to "), Var "b", Val (StringVal " ")])
    ]

-- >>> runTestTT test_stringSub
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

-- Parser used for command arguments
commandStringP :: Parser Expression
commandStringP = (Val <$> stringNoSubSpaceP) <|> (Var <$> wsP' (P.char '$' *> nameP)) <|> stringSubP

-- This will be used when a variable is assigned a command expression
commandExpressionP :: Parser Expression
commandExpressionP =
  CommandExpression <$> P.filter (`notElem` (Val . StringVal <$> reserved)) (Val <$> stringNoSubSpaceP) <*> many commandStringP

test_command :: Test
test_command =
  TestList
    [ P.parse (many commandStringP) "$val \"a string with $val\" someRegString"
        ~?= Right
          [ Var "val",
            StringSub [Val (StringVal "a string with "), Var "val"],
            Val (StringVal "someRegString")
          ],
      P.parse commandExpressionP "commandName \"$a1 neq $b2.\" $c something\nvar=10"
        ~?= Right
          ( CommandExpression
              (Val (StringVal "commandName"))
              [ StringSub
                  [ Var "a1",
                    Val (StringVal " neq "),
                    Var "b2",
                    Val (StringVal ".")
                  ],
                Var "c",
                Val (StringVal "something")
              ]
          ),
      P.parse stringStopSubP "\"a\\\"\"" ~?= Left "No parses",
      P.parse stringStopSubP "The value is $a" ~?= Right (StringVal "The value is "),
      P.parse stringNoSubSpaceP "echo 1 2" ~?= Right (StringVal "echo"),
      P.parse stringSubP "\"$a is neq to $b \" " ~?= Right (StringSub [Var "a", Val (StringVal " is neq to "), Var "b", Val (StringVal " ")])
    ]

-- >>> runTestTT test_command
-- Counts {cases = 6, tried = 6, errors = 0, failures = 0}

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

-- Var assignment in the shell is strict, i.e. e=1 parses but e = 1 does not
nameP :: Parser Name
nameP =
  P.filter
    (`notElem` reserved)
    ( (:)
        <$> P.choice [P.upper, P.lower, P.char '_']
        <*> many (P.choice [P.upper, P.lower, P.digit, P.char '_'])
    )

uopP :: Parser Uop
uopP =
  wsP
    ( P.choice
        [ constP "!" Not,
          constP "-z" DashZLen,
          constP "-n" DashNLen
        ]
    )

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

forP :: Parser Statement
forP =
  For
    <$> (stringP "for" *> wsP varP)
    <*> (stringP "in" *> wsP (some (stringValP' <|> intValP' <|> boolValP')))
    <*> (stringP "do" *> blockP <* stringP "done")

--- >>> P.parse blockP "# comment"
-- Right (Block [CommandStatement (Val (StringVal "#")) [Val (StringVal "comment")]])

statementP :: Parser Statement
statementP =
  wsP
    ( P.choice
        [ constP "continue" Continue,
          constP "break" Break,
          Assign
            <$> varP
            <*> (P.char '=' *> (expP <|> backticks commandExpressionP)),
          If
            <$> (stringP "if" *> expP)
            <*> (stringP "then" *> blockP <* stringP "fi"),
          IfElse
            <$> (stringP "if" *> expP)
            <*> (stringP "then" *> blockP)
            <*> (stringP "else" *> blockP <* stringP "fi"),
          While
            <$> (stringP "while" *> expP)
            <*> (stringP "do" *> blockP <* stringP "done"),
          forP,
          Until
            <$> (stringP "until" *> expP)
            <*> (stringP "do" *> blockP <* stringP "done"),
          constP "#" Comment <* many (P.satisfy (/= '\n')),
          CommandStatement
            <$> P.filter (`notElem` (Val . StringVal <$> reserved)) (Val <$> stringNoSubSpaceP)
            <*> wsP (many commandStringP)
        ]
    )

blockP :: Parser Block
blockP = Block <$> many statementP

-- >>> P.parse blockP "if [ $var1 -eq 2 -a $var2 -eq 0 ]\nthen\n  break\nelse\n  echo \"$var1 $var2\"\nfi"
-- Right (Block [])

parseShExp :: String -> Either P.ParseError Expression
parseShExp = P.parse expP

parseShStat :: String -> Either P.ParseError Statement
parseShStat = P.parse statementP

parseShFile :: String -> IO (Either P.ParseError Block)
parseShFile = P.parseFromFile (const <$> blockP <*> P.eof)

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
        P.parse (many expP) "`$a+5`" ~?= Right [Op2 (Var "a") Plus (Val (IntVal 5))],
        P.parse (backticks commandExpressionP) "`expr $a + 5`" ~?= Right (CommandExpression (Val (StringVal "expr")) [Var "a", Val (StringVal "+"), Val (StringVal "5")])
      ]

-- >>> runTestTT test_exp
-- Counts {cases = 11, tried = 11, errors = 0, failures = 0}

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
            ),
        P.parse statementP "val=`expr $a + $b`"
          ~?= Right
            ( Assign
                (Name "val")
                ( CommandExpression
                    (Val $ StringVal "expr")
                    [Var "a", Val $ StringVal "+", Var "b"]
                )
            ),
        P.parse statementP "for x in 1 2 3\ndo\n  echo $x\ndone"
          ~?= Right
            ( For
                (Name "x")
                [IntVal 1, IntVal 2, IntVal 3]
                (Block [CommandStatement (Val (StringVal "echo")) [Var "x"]])
            ),
        P.parse statementP "if [ $a != $b ]\nthen\n  echo \"a is not equal to b\"\nfi"
          ~?= Right
            ( If
                (Op2 (Var "a") Neq (Var "b"))
                ( Block
                    [ CommandStatement
                        (Val (StringVal "echo"))
                        [StringSub [Val (StringVal "a is not equal to b")]]
                    ]
                )
            )
      ]

-- >>> runTestTT test_stat
-- Counts {cases = 5, tried = 5, errors = 0, failures = 0}

test_all :: IO Counts
test_all = runTestTT $ TestList [test_value, test_exp, test_stat] -- test_comb

-- >>> test_all
-- Counts {cases = 25, tried = 25, errors = 0, failures = 0}
