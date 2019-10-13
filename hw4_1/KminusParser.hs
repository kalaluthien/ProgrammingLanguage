module KminusParser ( parseSource, parseFile ) where

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language 
import Text.Parsec.Expr

data Expr = UnitVal
          | Assign Expr Expr
          | Seq Expr Expr
          | Branch Expr Expr Expr
          | Loop Expr Expr
          | Read Expr
          | Write Expr
          | BindVar Expr Expr Expr
          | BindProc Expr [Expr] Expr Expr
          | CallVal Expr [Expr]
          | CallRef Expr [Expr]
          | IntVal Integer
          | BoolVal Bool
          | Record [Expr]
          | Ident String
          | DotOp Expr Expr
          | NotOp Expr
          | MulOp Expr Expr
          | DivOp Expr Expr
          | AddOp Expr Expr
          | SubOp Expr Expr
          | EqOp Expr Expr
          | LessOp Expr Expr
          deriving (Eq, Show)

languageDef :: LanguageDef st
languageDef = emptyDef
              { commentStart = "(*"
              , commentEnd = "*)"
              , identStart = letter
              , identLetter = alphaNum
              , reservedOpNames = [ "<", "=", ":=", ";"
                                  , "+", "-", "*", "/"
                                  , "not", ".", "write"]
              , reservedNames = [ "unit", "true", "false"
                                , "if", "then", "else"
                                , "let", "in", "proc"
                                , "while", "do", "not"
                                , "read", "write" ]
              , nestedComments = True
              , caseSensitive = True
              }

lexer = makeTokenParser languageDef
whiteSpaceLexer = whiteSpace lexer
parensLexer = parens lexer -- "( )"
anglesLexer = angles lexer -- "< >"
bracesLexer = braces lexer -- "{ }"
commaSepLexer = commaSep lexer -- ","
integerLexer = (char '-' >> return negate) <*> natural lexer <|> natural lexer
identifierLexer = identifier lexer
reservedOpLexer = reservedOp lexer
reservedLexer = reserved lexer

exprParser :: Parser Expr
exprParser = buildExpressionParser table term <?> "expression"
  where table = [ [ Infix (reservedOpLexer "." >> return DotOp) AssocLeft ]
                , [ Prefix (reservedOpLexer "not" >> return NotOp) ]
                , [ Infix (reservedOpLexer "*" >> return MulOp) AssocLeft
                  , Infix (reservedOpLexer "/" >> return DivOp) AssocLeft ]
                , [ Infix (reservedOpLexer "+" >> return MulOp) AssocLeft
                  , Infix (reservedOpLexer "-" >> return SubOp) AssocLeft ]
                , [ Infix (reservedOpLexer "=" >> return EqOp) AssocLeft
                  , Infix (reservedOpLexer "<" >> return LessOp) AssocLeft ]
                , [ Prefix (reservedOpLexer "write" >> return Write) ]
                , [ Infix (reservedOpLexer ":=" >> return Assign) AssocRight ]
                , [ Infix (reservedOpLexer ";" >> return Seq) AssocLeft ]
                ]
        term = parensLexer exprParser
               <|> unitParser
               <|> trueParser
               <|> falseParser
               <|> integerParser
               <|> readParser
               <|> callOrIdentParser
               <|> bindVarOrProcParser
               <|> branchParser
               <|> loopParser
               <|> recordParser
               <?> "term"
          where identParser = identifierLexer >>= return . Ident
                unitParser = reservedLexer "unit" >> return UnitVal
                trueParser = reservedLexer "true" >> return (BoolVal True)
                falseParser = reservedLexer "false" >> return (BoolVal False)
                integerParser = integerLexer >>= return . IntVal
                readParser = do
                  reservedLexer "read"
                  v <- identParser
                  return . Read $ v
                branchParser = do
                  reservedLexer "if"
                  b <- exprParser
                  reservedLexer "then"
                  p <- exprParser
                  reservedLexer "else"
                  q <- exprParser
                  return $ Branch b p q
                loopParser = do
                  reservedLexer "while"
                  b <- exprParser
                  reservedLexer "do"
                  e <- exprParser
                  return $ Loop b e
                bindVarParser = do
                  v <- identParser
                  reservedOpLexer ":="
                  p <- exprParser
                  reservedLexer "in"
                  q <- exprParser
                  return $ BindVar v p q
                bindProcParser = do
                  reservedLexer "proc"
                  f <- identParser
                  l <- parensLexer $ commaSepLexer identParser
                  reservedOpLexer "="
                  p <- exprParser
                  reservedLexer "in"
                  q <- exprParser
                  return $ BindProc f l p q
                bindVarOrProcParser = do
                  reservedLexer "let"
                  bindVarParser <|> bindProcParser
                callByValParser f = do
                  l <- parensLexer $ commaSepLexer exprParser
                  return $ CallVal f l
                callByRefParser f = do
                  l <- anglesLexer $ commaSepLexer identParser
                  return $ CallRef f l
                callOrIdentParser = do
                  f <- identParser
                  callByValParser f <|> callByRefParser f <|> return f
                assignParser = do
                  v <- identParser
                  reservedOpLexer ":="
                  e <- exprParser
                  return $ Assign v e
                recordParser = do
                  l <- bracesLexer $ commaSepLexer assignParser
                  return $ Record l

parseSource :: String -> IO Expr
parseSource source = do
  go $ parse (whiteSpaceLexer >> exprParser) "" source
    where go (Left e) = print e >> fail "<<< k- Parse Error >>>"
          go (Right r) = return r

parseFile :: String -> IO Expr
parseFile file = readFile file >>= parseSource
