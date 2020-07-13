{-#LANGUAGE LambdaCase#-}
module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad.Except

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Str String
             | Boolean Bool

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving Show

type ThrowsError = Either LispError

instance Show LispVal where
    show = showLisp

showLisp :: LispVal -> String
showLisp (Atom x        ) = x
showLisp (List x        ) = "(" ++ unwordsList x ++ ")"
showLisp (DottedList x y) = "(" ++ unwordsList x ++ " . " ++ showLisp y ++ ")"
showLisp (Number  x     ) = show x
showLisp (Str     x     ) = "\"" ++ x ++ "\""
showLisp (Boolean x     ) = if x then "#t" else "#f"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLisp

trapError a = catchError a (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x

main :: IO ()
main =
    putStr "> "
        >>  getLine
        >>= ( putStrLn
            . extractValue
            . trapError
            . fmap show
            . (readExpr >=> eval)
            )
        >>  main

readExpr :: String -> ThrowsError LispVal
readExpr x = case regularParse parseExpr x of
    Left  err -> throwError $ Parser err
    Right val -> return val

{-
The parser part
-}

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@~_^#"

spaces :: Parser ()
spaces = skipMany1 space

parseExpr :: Parser LispVal
parseExpr = choice
    [ parseAtom
    , parseStr
    , parseNumber
    , parseQuoted
    , char '(' *> choice [try parseList, parseDotted] <* char ')'
    ]

parseAtom :: Parser LispVal
parseAtom = do
    first <- choice [letter, symbol]
    rest  <- many $ choice [letter, digit, symbol]
    let atom = first : rest
    return $ case atom of
        "#t" -> Boolean True
        "#f" -> Boolean False
        _    -> Atom atom

parseStr :: Parser LispVal
parseStr = Str <$> (char '\"' *> (many . noneOf) "\"" <* char '\"')

parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDotted :: Parser LispVal
parseDotted =
    DottedList <$> endBy parseExpr spaces <*> (char '.' >> spaces >> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = (\x -> List [Atom "quote", x]) <$> (char '\'' *> parseExpr)

{-
The evaluation part
-}

eval :: LispVal -> ThrowsError LispVal
eval val@(Str _) = return val
eval val@(Number _) = return val
eval val@(Boolean _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, conseq, alt]) = eval cond >>= \case
    Boolean False -> eval alt
    _             -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval _ = throwError $ Default "cannot evaluate"

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
    maybe (throwError $ NotFunction "X p-func args" func) ($ args)
        $ lookup func primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"        , numBinop (+))
    , ("-"        , numBinop (-))
    , ("/"        , numBinop div)
    , ("mod"      , numBinop mod)
    , ("quotient" , numBinop quot)
    , ("remainder", numBinop rem)
    ]

numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinop op params = Number . foldl1 op <$> mapM unpackNum params

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (Str s) = return s
unpackStr notStr  = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Boolean b) = return b
unpackBool notBool     = throwError $ TypeMismatch "bool" notBool

boolBinop
    :: (LispVal -> ThrowsError a)
    -> (a -> a -> Bool)
    -> [LispVal]
    -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else (\x y -> Boolean (x `op` y)) <$> unpacker (head args) <*> unpacker
        (args !! 1)

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool
