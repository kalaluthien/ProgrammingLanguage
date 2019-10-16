module Kminus ( KExpr
              , runProgram
              , parseSource
              , parseFile
              ) where

import Data.Bool
import Data.List

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language 
import Text.Parsec.Expr

{- Location Type -}
newtype Loc = Loc { getLoc :: Int } deriving (Eq, Show)

baseLoc :: Loc
baseLoc = Loc 0

diffLoc :: Loc -> Loc -> Int
diffLoc l1 l2 = getLoc l1 - getLoc l2

incLoc :: Loc -> Int -> Loc
incLoc l n = Loc $ getLoc l + n

{- Memory Type -}
type Mem a = (Loc, [Maybe a])

emptyMem :: Mem a
emptyMem = (baseLoc, [])

loadMem :: Mem a -> Loc -> a
loadMem (ltop, s) loc = go s (diffLoc ltop loc)
  where go [] _ = error "NotAllocated"
        go (Nothing : _) 1 = error "NotInitialized"
        go (Just x : _) 1 = x
        go (_:xs) n = go xs (n - 1)

storeMem :: Mem a -> Loc -> a -> Mem a
storeMem (ltop, s) loc v = (ltop, go s (diffLoc ltop loc) v)
  where go [] _ _ = error "NotAllocated"
        go (_:xs) 1 v = Just v : xs
        go (x:xs) n v = x : go xs (n - 1) v

allocMem :: Mem a -> (Loc, Mem a)
allocMem (l, s) = (l, (incLoc l 1, Nothing : s))

{- Environment Type -}
type Env a b = ([a], a -> b)

emptyEnv :: Env a b
emptyEnv = ([], \_ -> error "NotBound")

lookupEnv :: Env a b -> a -> b
lookupEnv (_, f) x = f x

bindEnv :: (Eq a) => Env a b -> a -> b -> Env a b
bindEnv (l, f) x y = (x : l, \v -> if x == v then y else f v)

{- K- Interpreter -}
data KVal = UnitVal
          | BoolVal Bool
          | NumVal Int
          | RecordVal (String -> Loc)

type KMem = Mem KVal

data EnvEntry = Addr Loc | Proc [String] KExpr (Env String EnvEntry)

type KEnv = Env String EnvEntry

runProgram :: KExpr -> KVal
runProgram program = result
  where (result, _) = evalProgram emptyMem emptyEnv program

evalProgram :: KMem -> KEnv -> KExpr -> (KVal, KMem)
evalProgram mem env = eval
  where eval (Read e) = undefined
        eval (Write e) = undefined
        eval (BindVar e1 e2 e3) = undefined
        eval (Assign e1 e2) = undefined
        eval _ = error "Unimplemented"

valueAsUnit :: KVal -> ()
valueAsUnit UnitVal = ()
valueAsUnit _ = error "TypeError: not unit"

valueAsBool :: KVal -> Bool
valueAsBool (BoolVal b) = b
valueAsBool _ = error "TypeError: not bool"

valueAsNum :: KVal -> Int
valueAsNum (NumVal n) = n
valueAsNum _ = error "TypeError: not int"

valueAsRecord :: KVal -> (String -> Loc)
valueAsRecord (RecordVal r) = r
valueAsRecord _ = error "TypeError: not record"

lookupEnvLoc :: KEnv -> String -> Loc
lookupEnvLoc e x = go $ lookupEnv e x
  where go (Addr v) = v
        go (Proc _ _ _) = error "TypeError: not addr"

lookupEnvProc :: KEnv -> String -> ([String], KExpr, KEnv)
lookupEnvProc e f = go $ lookupEnv e f
  where go (Addr _) = error "TypeError: not proc"
        go (Proc ids expr env) = (ids, expr, env)

{- K- Parser -}
data KExpr = UnitExpr
           | BoolExpr Bool
           | NumExpr Integer
           | Var String
           | Seq KExpr KExpr
           | Assign KExpr KExpr
           | Branch KExpr KExpr KExpr
           | Loop KExpr KExpr
           | Read KExpr
           | Write KExpr
           | BindVar KExpr KExpr KExpr
           | BindProc KExpr [KExpr] KExpr KExpr
           | CallVal KExpr [KExpr]
           | CallRef KExpr [KExpr]
           | Record [KExpr]
           | DotOp KExpr KExpr
           | NotOp KExpr
           | MulOp KExpr KExpr
           | DivOp KExpr KExpr
           | AddOp KExpr KExpr
           | SubOp KExpr KExpr
           | EqOp KExpr KExpr
           | LessOp KExpr KExpr
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
parensLexer = parens lexer
anglesLexer = angles lexer
bracesLexer = braces lexer
commaSepLexer = commaSep lexer
integerLexer = (char '-' >> return negate)
               <*> natural lexer
               <|> natural lexer
identifierLexer = identifier lexer
reservedOpLexer = reservedOp lexer
reservedLexer = reserved lexer

exprParser :: Parser KExpr
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
               <|> callOrVarParser
               <|> bindVarOrProcParser
               <|> branchParser
               <|> loopParser
               <|> recordParser
               <?> "term"
          where identParser = identifierLexer >>= return . Var
                unitParser = reservedLexer "unit" >> return UnitExpr
                trueParser = reservedLexer "true" >> return (BoolExpr True)
                falseParser = reservedLexer "false" >> return (BoolExpr False)
                integerParser = integerLexer >>= return . NumExpr
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
                callOrVarParser = do
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

parseSource :: String -> IO KExpr
parseSource source = do
  go $ parse (whiteSpaceLexer >> exprParser) "" source
    where go (Left e) = print e >> fail "<<< k- Parse Error >>>"
          go (Right r) = return r

parseFile :: String -> IO KExpr
parseFile file = readFile file >>= parseSource
