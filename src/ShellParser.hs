module ShellParser where

import Control.Applicative
import Data.Char (isSpace)
import Data.Char qualified as Char
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

dollarP :: Parser Expression
dollarP = P.char '$' *> expP

expressionListP :: Parser [Expression]
expressionListP = many (dollarP <|> expP)

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

stringValP :: Parser Value
stringValP =
  StringVal
    <$> wsP (escQuotes (many (P.satisfy ('\"' /=))))

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

stringNoSpaceP :: Parser String
stringNoSpaceP =
  wsP
    ( (:)
        <$> P.satisfy (\c -> (not . Char.isSpace) c && c /= '\"')
        <*> many (P.satisfy (\c -> (not . Char.isSpace) c && c /= '\"'))
    )

-- >>> P.parse stringNoSpaceP "echo 1 2"
-- Right "echo"

-- >>> P.parse (many stringNoSpaceP) "echo 1 2"
-- Right ["echo","1","2"]

stringLiteralP :: Parser String
stringLiteralP =
  wsP (escQuotes (many (P.satisfy ('\"' /=)))) <|> stringNoSpaceP

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
      Var <$> (P.char '$' *> nameP)
        <|> parens expP -- ()
        <|> brackets expP -- []
        <|> Val <$> valueP

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
    "expr", -- evaluate expression
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
        [ constP "not" Not
        -- ,
        -- constP "#" Len
        ]
    )

-- >>> P.parse (many bopP) "+ >= .."
-- Right [Plus,Ge,Concat]
bopP :: Parser Bop
bopP =
  wsP
    ( P.choice
        [ constP "+" Plus,
          constP "-" Minus,
          constP "\\*" Times,
          constP "//" Divide,
          constP "%" Modulo,
          constP "==" Eq,
          constP "-eq" Eq,
          constP ">=" Ge,
          constP "-ge" Ge,
          constP ">" Gt,
          constP "-gt" Gt,
          constP "<=" Le,
          constP "-le" Le,
          constP "<" Lt,
          constP "-lt" Lt,
          constP ".." Concat
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
                                 <$> stringLiteralP
                                 <*> many stringLiteralP
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
          CommandStatement <$> stringLiteralP <*> many stringLiteralP
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

test_comb =
  "parsing combinators"
    ~: TestList
      [ P.parse (wsP P.alpha) "a" ~?= Right 'a',
        P.parse (many (wsP P.alpha)) "a b \n   \t c" ~?= Right "abc",
        P.parse (stringP "a") "a" ~?= Right (),
        P.parse (stringP "a") "b" ~?= Left "No parses",
        P.parse (many (stringP "a")) "a  a" ~?= Right [(), ()],
        P.parse (constP "&" 'a') "&  " ~?= Right 'a',
        P.parse (many (constP "&" 'a')) "&   &" ~?= Right "aa",
        P.parse (many (brackets (constP "1" 1))) "[1] [  1]   [1 ]" ~?= Right [1, 1, 1]
      ]

-- test_value =
--   "parsing values"
--     ~: TestList
--       [ P.parse (many intValP) "1 2\n 3" ~?= Right [IntVal 1, IntVal 2, IntVal 3],
--         P.parse (many boolValP) "true false\n true" ~?= Right [BoolVal True, BoolVal False, BoolVal True],
--         P.parse (many nilValP) "nil nil\n nil" ~?= Right [NilVal, NilVal, NilVal],
--         P.parse stringValP "\"a\"" ~?= Right (StringVal "a"),
--         P.parse stringValP "\"a\\\"\"" ~?= Right (StringVal "a\\"),
--         P.parse (many stringValP) "\"a\"   \"b\"" ~?= Right [StringVal "a", StringVal "b"],
--         P.parse (many stringValP) "\" a\"   \"b\"" ~?= Right [StringVal " a", StringVal "b"]
--       ]

-- test_exp =
--   "parsing expressions"
--     ~: TestList
--       [ P.parse (many varP) "x y z" ~?= Right [Name "x", Name "y", Name "z"],
--         P.parse varP "(x.y[1]).z" ~?= Right (Dot (Var (Proj (Var (Dot (Var (Name "x")) "y")) (Val (IntVal 1)))) "z"),
--         P.parse (many nameP) "x sfds _ nil" ~?= Right ["x", "sfds", "_"],
--         P.parse (many uopP) "- - #" ~?= Right [Neg, Neg, Len],
--         P.parse (many bopP) "+ >= .." ~?= Right [Plus, Ge, Concat],
--         P.parse tableConstP "{ x = 2, [3] = false }"
--           ~?= Right (TableConst [FieldName "x" (Val (IntVal 2)), FieldKey (Val (IntVal 3)) (Val (BoolVal False))])
--       ]

test_stat =
  "parsing statements"
    ~: TestList
      [ -- P.parse statementP ";" ~?= Right Empty,
        P.parse statementP "VAR=3" ~?= Right (Assign (Name "VAR") (Val (IntVal 3)))
        -- ,
        --   P.parse statementP "if x then y=nil else end"
        --     ~?= Right (If (Var (Name "x")) (Block [Assign (Name "y") (Val NilVal)]) (Block [])),
        --   P.parse statementP "while nil do end"
        --     ~?= Right (While (Val NilVal) (Block [])),
        --   P.parse statementP "repeat ; ; until false"
        --     ~?= Right (Repeat (Block [Empty, Empty]) (Val (BoolVal False)))
      ]

-- >>> runTestTT test_stat
-- Counts {cases = 1, tried = 1, errors = 0, failures = 0}

-- test_all :: IO Counts
-- test_all = runTestTT $ TestList [test_comb, test_value, test_exp, test_stat, tParseFiles]
