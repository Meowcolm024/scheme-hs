{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )
import           Control.Monad.Except
import           System.IO
import           System.Environment             ( getArgs )
import           Data.IORef
import           Data.Maybe                     ( isJust
                                                , isNothing
                                                )

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Str String
             | Boolean Bool
             | PreFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

type ThrowsError = Either LispError

type Env = IORef [(String, IORef LispVal)]

type IOThrowsError = ExceptT LispError IO

instance Show LispVal where
    show = showLisp

instance Eq LispVal where
    Atom a         == Atom b         = a == b
    List a         == List b         = a == b
    DottedList a c == DottedList b d = a == b && c == d
    Number  a      == Number  b      = a == b
    Str     a      == Str     b      = a == b
    Boolean a      == Boolean b      = a == b
    _              == _              = False

instance Show LispError where
    show = showError

showLisp :: LispVal -> String
showLisp (Atom x        ) = x
showLisp (List x        ) = "(" ++ unwordsList x ++ ")"
showLisp (DottedList x y) = "(" ++ unwordsList x ++ " . " ++ showLisp y ++ ")"
showLisp (Number  x     ) = show x
showLisp (Str     x     ) = "\"" ++ x ++ "\""
showLisp (Boolean x     ) = if x then "#t" else "#f"
showLisp (PreFunc _     ) = "<primitive>"
showLisp Func{}           = "<function>"
showLisp (IOFunc _)       = "<IO primitive>"
showLisp (Port   _)       = "<IO port>"

showError :: LispError -> String
showError (UnboundVar     message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction    message func   ) = message ++ ": " ++ show func
showError (NumArgs expected found) =
    " Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) =
    "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser  parseErr) = " Parse error at " ++ show parseErr
showError (Default err     ) = "Error: " ++ err

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showLisp

trapError :: (MonadError a m, Show a) => m String -> m String
trapError a = catchError a (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right x) = x
extractValue (Left  _) = error "How does it even possible?"

{-
IO part
-}

main :: IO ()
main = do
    args <- getArgs
    if null args then flushStr "Scheme\n" >> runRepl else runOne args

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case regularParse parser input of
    Left  err -> throwError $ Parser err
    Right val -> return val

readExpr = readOrThrow parseExpr
readExprList = readOrThrow $ endBy parseExpr spaces

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString =
    ((runIOThrows . fmap show) .) . flip ((>>=) . liftThrows . readExpr) . eval

evalAndPrint :: Env -> String -> IO ()
evalAndPrint = ((putStrLn =<<) .) . evalString

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ cond prompt act = prompt
    >>= \r -> if cond r then return () else act r >> until_ cond prompt act

runRepl :: IO ()
runRepl =
    preBindings
        >>= until_ (\x -> x == "quit" || x == "#q") (readPrompt "> ")
        .   evalAndPrint

runOne :: [String] -> IO ()
runOne args = do
    env <- preBindings >>= flip bindVars [("args", List $ map Str $ tail args)]
    runIOThrows (show <$> eval env (List [Atom "load", Str (head args)]))
        >>= hPutStrLn stderr

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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _   val@(Str     _                             ) = return val
eval _   val@(Number  _                             ) = return val
eval _   val@(Boolean _                             ) = return val
eval env (    Atom    id'                           ) = getVar env id'
eval _   (    List    [Atom "quote", val]           ) = return val
eval env (    List    [Atom "if", cond, conseq, alt]) = eval env cond >>= \case
    Boolean False -> eval env alt
    _             -> eval env conseq
eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params') : body')) =
    makeNormalFunc env params' body' >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params') varargs' : body'))
    = makeVarargs varargs' env params' body' >>= defineVar env var
eval env (List (Atom "lambda" : List params' : body')) =
    makeNormalFunc env params' body'
eval env (List (Atom "lambda" : DottedList params' varargs' : body')) =
    makeVarargs varargs' env params' body'
eval env (List (Atom "lambda" : varargs'@(Atom _) : body')) =
    makeVarargs varargs' env [] body'
eval env (List [Atom "load", Str filename]) =
    load filename >>= fmap last . mapM (eval env)
eval env (List (func : args)) = do
    f <- eval env func
    a <- mapM (eval env) args
    apply f a
eval _ badForm =
    throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PreFunc func) args = liftThrows $ func args
apply (Func params' varargs' body' closure') args =
    if num params' /= num args && isNothing varargs'
        then throwError $ NumArgs (num params') args
        else
            liftIO (bindVars closure' $ zip params' args)
            >>= bindVarArgs varargs'
            >>= evalBody
  where
    rmArgs = drop (length params') args
    num    = toInteger . length
    evalBody env = last <$> mapM (eval env) body'
    bindVarArgs arg env = case arg of
        Just argName -> liftIO $ bindVars env [(argName, List rmArgs)]
        Nothing      -> return env
apply nf _ = throwError $ NotFunction "Not Function" (show nf)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+"        , numBinop (+))
    , ("-"        , numBinop (-))
    , ("*"        , numBinop (*))
    , ("/"        , numBinop div)
    , ("mod"      , numBinop mod)
    , ("quotient" , numBinop quot)
    , ("remainder", numBinop rem)
    , ("="        , numBoolBinop (==))
    , (">"        , numBoolBinop (>))
    , ("<"        , numBoolBinop (<))
    , ("/="       , numBoolBinop (/=))
    , (">="       , numBoolBinop (>=))
    , ("<="       , numBoolBinop (<=))
    , ("&&"       , boolBoolBinop (&&))
    , ("||"       , boolBoolBinop (||))
    , ("string=?" , strBoolBinop (==))
    , ("car"      , car)
    , ("cdr"      , cdr)
    , ("cons"     , cons)
    , ("eq?"      , eqv)
    ]

numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinop op params' = Number . foldl1 op <$> mapM unpackNum params'

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [badArg              ] = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs        ] = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [a, b              ] = return $ DottedList [a] b
cons badArgList           = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [x, y]     = return $ Boolean (x == y)
eqv badArgList = throwError $ NumArgs 2 badArgList

{-
var part
-}

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = extractValue <$> runExceptT (trapError action)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = (liftIO . readIORef) envRef >>= \env -> maybe
    (throwError $ UnboundVar "Unbound var" var)
    (liftIO . readIORef)
    (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Unbound var" var)
          (liftIO . flip writeIORef value)
          (lookup var env)
    return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
    ad <- liftIO $ isBound envRef var
    if ad
        then setVar envRef var value >> return value
        else liftIO $ do
            valueRef <- newIORef value
            env      <- readIORef envRef
            writeIORef envRef ((var, valueRef) : env)
            return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bind env = (++ env) <$> mapM addBinding bind
    addBinding (var, val) = (var, ) <$> newIORef val

{-
functions
-}

preBindings :: IO Env
preBindings =
    (>>=) nullEnv
        $  flip bindVars
        $  map (mf IOFunc)  ioPrimitives
        ++ map (mf PreFunc) primitives
    where mf c (v, f) = (v, c f)

makeFunc v e p b = return $ Func (map show p) v b e
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showLisp

{-
IO
-}

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
    [ ("apply"            , applyProc)
    , ("open-input-file"  , makePort ReadMode)
    , ("open-output-file" , makePort WriteMode)
    , ("close-input-port" , closePort)
    , ("close-output-port", closePort)
    , ("read"             , readProc)
    , ("write"            , writeProc)
    , ("read-contents"    , readContents)
    , ("read-all"         , readAll)
    ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [Str filename] = Port <$> liftIO (openFile filename mode)

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Boolean True)
closePort _           = return $ Boolean False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [x]            = writeProc [x, Port stdout]
writeProc [x, Port port] = liftIO $ hPrint port x >> return (Boolean True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [Str filename] = Str <$> liftIO (readFile filename)

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [Str n] = List <$> load n
