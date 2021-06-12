--
-- mscheme.hs : microScheme
--
--              Copyright (C) 2013 Makoto Hiroi
--
{--
   オリジナルは、以下で公開されたもので、
　「お気楽 Haskell プログラミング入門」
   http://www.nct9.ne.jp/m_hiroi/func/haskell.html

　　これを、Hakellの最新の環境に移植したものです。
--}

import Data.Char
import Data.IORef

import Control.Monad.Trans.Except
import System.IO

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

type ScmFunc = Env -> SExpr -> Scm SExpr

-- S 式の定義
data SExpr = INT  !Integer
           | REAL !Double
           | SYM  String
           | STR  String
           | CELL SExpr SExpr
           | NIL
           | PRIM ScmFunc
           | SYNT ScmFunc
           | CLOS SExpr LEnv
           | MACR SExpr

-- 等値の定義
instance Eq SExpr where
  INT x  == INT y  = x == y
  REAL x == REAL y = x == y
  SYM x  == SYM y  = x == y
  STR x  == STR y  = x == y
  NIL    == NIL    = True
  _      == _      = False

-- パーサエラーの定義
data ParseErr = ParseErr String String deriving Show

class Error a where
    noMsg :: a
    strMsg :: String -> a

instance Error ParseErr where
  noMsg    = ParseErr "" ""
  strMsg s = ParseErr "" s

-- パーサの定義
type Parser a = Either ParseErr a

-- 評価器の定義
type Scm a = ExceptT ParseErr IO a

-- ローカル環境の定義
type LEnv = [(String, IORef SExpr)]

pushLEnv :: String -> SExpr -> LEnv -> IO LEnv
pushLEnv s v env = do
  a <- v `seq` newIORef v
  return ((s, a):env)

lookupLEnv :: String -> LEnv -> IO (Maybe SExpr)
lookupLEnv s env = 
  case lookup s env of
    Nothing -> return Nothing
    Just v  -> do a <- readIORef v
                  return (Just a)

updateLEnv :: String -> SExpr -> LEnv -> IO (LEnv)
updateLEnv s v env =
  case lookup s env of
    Nothing -> pushLEnv s v env
    Just a  -> do writeIORef a v
                  return env

-- グローバルな環境
type GEnv = HashTable String SExpr

-- 両方の環境を保持する
type Env = (GEnv, LEnv)

-- 真偽値
true  = SYM "true"
false = SYM "false"

-- Primitive の定義
errNUM  = "Illegal argument, Number required"
errINT  = "Illegal argument, Integer required"
errNEA  = "Not enough arguments"
errCELL = "Illegal argument, List required"
errZERO = "Divide by zero"

-- リスト操作
car :: ScmFunc
car _ NIL = throwE $ strMsg $ "car : " ++ errNEA
car _ (CELL (CELL a _) _) = return a
car _ _                   = throwE $ strMsg $ "car : " ++ errCELL

cdr :: ScmFunc
cdr _ NIL = throwE $ strMsg $ "cdr : " ++ errNEA
cdr _ (CELL (CELL _ d) _) = return d
cdr _ _                   = throwE $ strMsg $ "cdr : " ++ errCELL

cons :: ScmFunc
cons _ (CELL a (CELL b _)) = return (CELL a b)
cons _ _                   = throwE $ strMsg $ "cons : " ++ errNEA

pair :: ScmFunc
pair _ NIL                 = throwE $ strMsg $ "pair? : " ++ errNEA
pair _ (CELL (CELL _ _) _) = return true
pair _ _                   = return false

-- 畳み込み
foldCell :: (SExpr -> SExpr -> Scm SExpr) -> SExpr -> SExpr -> Scm SExpr
foldCell _ a NIL = return a
foldCell f a (CELL x rest) = do v <- f a x
                                foldCell f v rest
foldCell _ _ _ = throwE $ strMsg $ errCELL

-- 四則演算
add :: SExpr -> SExpr -> Scm SExpr
add (INT x)  (INT y)  = return (INT (x + y))
add (INT x)  (REAL y) = return (REAL (fromIntegral x + y))
add (REAL x) (INT y)  = return (REAL (x + fromIntegral y))
add (REAL x) (REAL y) = return (REAL (x + y))
add _        _        = throwE $ strMsg $ "+ : " ++ errNUM

adds :: ScmFunc
adds _ xs = foldCell add (INT 0) xs

sub :: SExpr -> SExpr -> Scm SExpr
sub (INT x)  (INT y)  = return (INT (x - y))
sub (INT x)  (REAL y) = return (REAL (fromIntegral x - y))
sub (REAL x) (INT y)  = return (REAL (x - fromIntegral y))
sub (REAL x) (REAL y) = return (REAL (x - y))
sub _        _        = throwE $ strMsg $ "- : " ++ errNUM

subs :: ScmFunc
subs _ NIL = throwE $ strMsg $ "- : " ++ errNEA
subs _ (CELL (INT a) NIL)  = return (INT (-a))
subs _ (CELL (REAL a) NIL) = return (REAL (-a))
subs _ (CELL a rest) = foldCell sub a rest

mul :: SExpr -> SExpr -> Scm SExpr
mul (INT x)  (INT y)  = return (INT (x * y))
mul (INT x)  (REAL y) = return (REAL (fromIntegral x * y))
mul (REAL x) (INT y)  = return (REAL (x * fromIntegral y))
mul (REAL x) (REAL y) = return (REAL (x * y))
mul _        _        = throwE $ strMsg $ "- : " ++ errNUM

muls :: ScmFunc
muls _ xs = foldCell mul (INT 1) xs

div' :: SExpr -> SExpr -> Scm SExpr
div' _        (INT 0)  = throwE $ strMsg errZERO
div' _        (REAL 0) = throwE $ strMsg errZERO
div' (INT x)  (INT y)  = return (INT (x `div` y))
div' (INT x)  (REAL y) = return (REAL (fromIntegral x / y))
div' (REAL x) (INT y)  = return (REAL (x / fromIntegral y))
div' (REAL x) (REAL y) = return (REAL (x / y))
div' _        _        = throwE $ strMsg $ "- : " ++ errNUM

divs :: ScmFunc
divs _ NIL = throwE $ strMsg $ "/ : " ++ errNEA
divs _ (CELL a NIL)  = div' (INT 1) a
divs _ (CELL a rest) = foldCell div' a rest

mod' :: ScmFunc
mod' _ NIL          = throwE $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ NIL) = throwE $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ (CELL (INT 0) _))  = throwE $ strMsg errZERO
mod' _ (CELL _ (CELL (REAL 0) _)) = throwE $ strMsg errZERO
mod' _ (CELL (INT x) (CELL (INT y) _)) = return (INT (mod x y))
mod' _ _ = throwE $ strMsg $ "mod : " ++ errINT

-- 等値の判定
eq' :: ScmFunc
eq' _ (CELL x (CELL y _)) =
  if x == y then return true else return false
eq' _ _ = throwE $ strMsg $ "eq : " ++ errNEA

equal' :: ScmFunc
equal' _ (CELL x (CELL y _)) =
  if iter x y then return true else return false
  where iter (CELL a b) (CELL c d) = iter a c && iter b d
        iter x y = x == y
equal' _ _ = throwE $ strMsg $ "equal : " ++ errNEA

-- 数値の比較演算子
compareNum :: SExpr -> SExpr -> Scm Ordering
compareNum (INT x)  (INT y)  = return $ compare x y
compareNum (INT x)  (REAL y) = return $ compare (fromIntegral x) y
compareNum (REAL x) (INT y)  = return $ compare x (fromIntegral y)
compareNum (REAL x) (REAL y) = return $ compare x y
compareNum _ _ = throwE $ strMsg errNUM

compareNums :: (Ordering -> Bool) -> SExpr -> Scm SExpr
compareNums _ NIL          = throwE $ strMsg errNEA
compareNums _ (CELL _ NIL) = throwE $ strMsg errNEA
compareNums p (CELL x (CELL y NIL)) = do
  r <- compareNum x y
  if p r then return true else return false
compareNums p (CELL x ys@(CELL y _)) = do
  r <- compareNum x y
  if p r then compareNums p ys else return false
compareNums _ _ = throwE $ strMsg "invalid function form"

eqNum, ltNum, gtNum, ltEq, gtEq :: ScmFunc
eqNum _ = compareNums (== EQ)
ltNum _ = compareNums (== LT)
gtNum _ = compareNums (== GT)
ltEq  _ = compareNums (<= EQ)
gtEq  _ = compareNums (>= EQ)


-- apply 

apply' :: ScmFunc
apply' _ (CELL _ NIL) = throwE $ strMsg $ "apply : " ++ errNEA
apply' env (CELL func args) = do
  xs <- iter args
  apply env func xs
  where iter (CELL NIL NIL) = return NIL
        iter (CELL xs@(CELL _ _) NIL) = return xs
        iter (CELL _ NIL) = throwE $ strMsg errCELL
        iter (CELL x xs) = do ys <- iter xs
                              return (CELL x ys)
apply' _ _ = throwE $ strMsg $ "apply : " ++ errNEA

-- error

error' :: ScmFunc
error' _ (CELL (STR x) NIL) = throwE $ strMsg $ "ERROR: " ++ x
error' _ (CELL (STR x) (CELL y _)) = throwE $ strMsg $ "ERROR: " ++ x ++ " " ++ show y
error' _ (CELL x _) = throwE $ strMsg $ "ERROR: " ++ show x
error' _ _ = throwE $ strMsg "ERROR: "

-- load

load :: ScmFunc
load env (CELL (STR filename) _) = do
  xs <- lift $ readFile filename
  r <- lift $ iter xs
  if r then return true else return false
  where
    iter :: String -> IO Bool
    iter xs = 
      case readSExpr xs of
        Left  (ParseErr xs' mes) -> if mes == "EOF"
                                      then return True
                                      else do print mes
                                              return False
        Right (expr, xs') -> do result <- runExceptT $ eval env expr 
                                case result of
                                  Left mes -> do print mes
                                                 return False
                                  Right _  -> iter xs'
load _ _ = throwE $ strMsg "invalid load form"


--
-- S 式の表示
--
showCell :: SExpr -> String
showCell (CELL a d) =
  show a ++ case d of
              NIL      -> ""
              PRIM _   -> "<primitive>"
              CLOS _ _ -> "<closure>"
              SYNT _   -> "<syntax>"
              INT x    -> " . " ++ show x
              REAL x   -> " . " ++ show x
              SYM x    -> " . " ++ x
              STR x    -> " . " ++ show x
              _        -> " " ++ showCell d
showCell xs = show xs

instance Show SExpr where
  show (INT x)    = show x
  show (REAL x)   = show x
  show (SYM x)    = x
  show (STR x)    = show x
  show NIL        = "()"
  show (SYNT _)   = "<syntax>"
  show (PRIM _)   = "<primitive>"
  show (CLOS _ _) = "<closure>"
  show xs         = "(" ++ showCell xs ++ ")"

--
-- S 式の読み込み
--

isAlpha' :: Char -> Bool
isAlpha' x = elem x "!$%&*+-/:<=>?@^_~"

isIdent0 :: Char -> Bool
isIdent0 x = isAlpha x || isAlpha' x

isIdent1 :: Char -> Bool
isIdent1 x = isAlphaNum x || isAlpha' x

isREAL :: Char -> Bool
isREAL x = elem x ".eE"

quote           = SYM "quote"
quasiquote      = SYM "quasiquote"
unquote         = SYM "unquote"
unquoteSplicing = SYM "unquote-splicing"

isNUM :: String -> Bool
isNUM (x:_) = isDigit x
isNUM _     = False

getNumber :: String -> Parser (SExpr, String)
getNumber xs =
  let (s, ys) = span isDigit xs
  in if not (null ys) && isREAL (head ys)
     then case reads xs of
            [] -> Left noMsg  -- ありえないエラー
            [(y', ys')] -> return (REAL y', ys')
     else return (INT (read s), ys)

readSExpr :: String -> Parser (SExpr, String)
readSExpr [] = Left $ strMsg "EOF"
readSExpr (x:xs) 
  | isSpace x  = readSExpr xs
  | isDigit x  = getNumber (x:xs)
  | isIdent0 x = if x == '+' && isNUM xs
                 then getNumber xs
                 else if x == '-' && isNUM xs
                 then do (y, ys) <- getNumber xs
                         case y of
                           INT x  -> return (INT  (- x), ys)
                           REAL x -> return (REAL (- x), ys)
                 else let (name, ys) = span isIdent1 (x:xs)
                      in return (SYM name, ys)
  | otherwise  =
      case x of
        '('  -> readCell 0 xs
        ';'  -> readSExpr $ dropWhile (/= '\n') xs
        '"'  -> case reads (x:xs) of
                  [] -> Left noMsg
                  [(y, ys)] -> return (STR y, ys)
        '\'' -> readSExpr xs >>= \(e, ys) -> return (CELL quote (CELL e NIL), ys)
        '`'  -> readSExpr xs >>= \(e, ys) -> return (CELL quasiquote (CELL e NIL), ys)
        ','  -> if not (null xs) && head xs == '@'
                  then readSExpr (tail xs) >>=
                       \(e, ys) -> return (CELL unquoteSplicing (CELL e NIL), ys)
                  else readSExpr xs >>=
                       \(e, ys) -> return (CELL unquote (CELL e NIL), ys)
        _    -> Left $ ParseErr xs ("unexpected token: " ++ show x)

readCell :: Int -> String -> Parser (SExpr, String)
readCell _ [] = Left $ strMsg "EOF"
readCell n (x:xs)
  | isSpace x = readCell n xs
  | otherwise =
      case x of
        ')' -> return (NIL, xs)
        '.' -> if n == 0
               then Left $ ParseErr xs "invalid dotted list"
               else do (e, ys) <- readSExpr xs
                       case dropWhile isSpace ys of
                         ')':zs -> return (e, zs)
                         _      -> Left $ ParseErr xs "invalid dotted list"
        '(' -> do (a, ys) <- readCell 0 xs
                  (d, zs) <- readCell 1 ys
                  return (CELL a d, zs)
        _   -> do (a, ys) <- readSExpr (x:xs)
                  (d, zs) <- readCell 1 ys
                  return (CELL a d, zs)


lift :: (Monad m) => m a -> ExceptT e m a
lift = ExceptT . liftM Right 
                     
-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f ma = do a <- ma
                return (f a)

liftIO = lift                

--
-- S 式の評価
--
eval :: Env -> SExpr -> Scm SExpr
eval env NIL        = return NIL
eval env v@(INT _)  = return v
eval env v@(REAL _) = return v
eval env v@(STR _)  = return v
eval env (SYM name) = do
  a <- liftIO $ lookupLEnv name $ snd env
  case a of
    Nothing -> do b <- liftIO $ H.lookup (fst env) name
                  case b of
                    Nothing -> throwE $ strMsg $ "unbound variable: " ++ name
                    Just v  -> return v
    Just v -> return v
eval env (CELL func args) = do
  v <- eval env func
  case v of
    SYNT f -> f env args
    MACR f -> do expr <- apply env f args
                 eval env expr
    _      -> do vs <- evalArguments env args
                 apply env v vs

-- 引数の評価
evalArguments :: Env -> SExpr -> Scm SExpr
evalArguments env NIL = return NIL
evalArguments env (CELL expr rest) = do
  v  <- eval env expr
  vs <- evalArguments env rest
  return (CELL v vs)
evalArguments _ _ = throwE $ strMsg "invalid function form"

-- 変数束縛
makeBindings :: LEnv -> SExpr -> SExpr -> Scm LEnv
makeBindings lenv NIL        _    = return lenv
makeBindings lenv (SYM name) rest = liftIO $ pushLEnv name rest lenv
makeBindings lenv (CELL (SYM name) parms) (CELL v args) = do
  lenv' <- makeBindings lenv parms args
  liftIO (pushLEnv name v lenv')
makeBindings lenv _ NIL = throwE $ strMsg errNEA
makeBindings lenv _ _   = throwE $ strMsg "invalid arguments form"

-- 関数適用
apply :: Env -> SExpr -> SExpr -> Scm SExpr
apply env func actuals =
  case func of
    PRIM f -> f env actuals
    CLOS (CELL parms body) lenv0 -> do
      lenv1 <- makeBindings lenv0 parms actuals
      evalBody (fst env, lenv1) body
    _ -> throwE $ strMsg $ "Not Function: " ++ show func

-- 本体の評価
evalBody :: Env -> SExpr -> Scm SExpr
evalBody env (CELL expr NIL) = eval env expr
evalBody env (CELL expr rest) = do
  eval env expr
  evalBody env rest
evalBody _ _ = throwE $ strMsg "invalid body form"

--
-- シンタックス形式
--

-- quote
evalQuote :: Env -> SExpr -> Scm SExpr
evalQuote env (CELL expr _) = return expr
evalQuote _ _ = throwE $ strMsg "invalid quote form"


-- define
evalDef :: Env -> SExpr -> Scm SExpr
evalDef env (CELL sym@(SYM name) (CELL expr NIL)) = do
  v <- eval env expr
  liftIO $ H.insert (fst env) name v
  return sym
evalDef _ _ = throwE $ strMsg "invalid define form"

-- define-macro
evalDefM :: Env -> SExpr -> Scm SExpr
evalDefM env (CELL sym@(SYM name) (CELL expr NIL)) = do
  v <- eval env expr
  liftIO $ H.insert (fst env) name (MACR v)
  return sym
evalDefM _ _ = throwE $ strMsg "invalid define form"


-- if
evalIf :: Env -> SExpr -> Scm SExpr
evalIf env (CELL pred (CELL thenForm rest)) = do
  v <- eval env pred
  if v /= false
  then eval env thenForm
  else case rest of
         CELL elseForm _ -> eval env elseForm
         _               -> return false
evalIf _ _ = throwE $ strMsg $ "if : " ++ errNEA

-- lambda
evalLambda :: Env -> SExpr -> Scm SExpr
evalLambda env expr = return (CLOS expr (snd env))

-- set!
evalSet :: Env -> SExpr -> Scm SExpr
evalSet env (CELL (SYM name) (CELL expr _)) = do
  v <- eval env expr
  a <- liftIO $ lookupLEnv name (snd env)
  case a of
    Nothing -> do b <- liftIO $ H.lookup (fst env) name
                  case b of
                    Nothing -> throwE $ strMsg $ "unbound variable: " ++ name
                    Just _ -> do liftIO $ H.insert (fst env) name v
                                 return v
    Just _  -> do liftIO $ updateLEnv name v (snd env)
                  return v
evalSet _ _ = throwE (strMsg "invalid set! form")

--
-- 大域変数の初期化
--
initGEnv :: [(String, SExpr)]
initGEnv = [("true",   true),
            ("false",  false),
            ("quote",  SYNT evalQuote),
            ("define", SYNT evalDef),
            ("lambda", SYNT evalLambda),
            ("if",     SYNT evalIf),
            ("set!",   SYNT evalSet),
            ("define-macro", SYNT evalDefM),
            ("eq?",    PRIM eq'),
            ("equal?", PRIM equal'),
            ("pair?",  PRIM pair),
            ("+",      PRIM adds),
            ("-",      PRIM subs),
            ("*",      PRIM muls),
            ("/",      PRIM divs),
            ("mod",    PRIM mod'),
            ("=",      PRIM eqNum),
            ("<",      PRIM ltNum),
            (">",      PRIM gtNum),
            ("<=",     PRIM ltEq),
            (">=",     PRIM gtEq),
            ("car",    PRIM car),
            ("cdr",    PRIM cdr),
            ("cons",   PRIM cons),
            ("apply",  PRIM apply'),
            ("error",  PRIM error'),
            ("load",   PRIM load)]

-- read-eval-print-loop
repl :: Env -> String -> IO ()
repl env xs = do
  putStr "Scm> "
  hFlush stdout
  case readSExpr xs of
    Left  (ParseErr xs' mes) -> do putStrLn mes
                                   if mes == "EOF"
                                     then return ()
                                     else repl env $ dropWhile (/= '\n') xs'
    Right (expr, xs') -> do result <- runExceptT $ eval env expr 
                            case result of
                              Left e -> putStrLn (show e)
                              Right v  -> print v
                            repl env xs'

--hashString = foldl' f golden
--   where f m c = fromIntegral (ord c) * magic + hashInt32 m
--         magic = 0xdeadbeef

main :: IO ()
main = do
  xs <- hGetContents stdin
  ht <- H.fromList initGEnv
  repl (ht, []) xs

