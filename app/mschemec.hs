--
-- MSchemec.hs : microScheme compiler
---

import Data.Char

import System.IO
import qualified Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except

import Evaluator
import Reader
import Compiler
import Error
import Primitives

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
            ("list",   PRIM list'),
            ("append", PRIM append'),
            ("error",  PRIM error'),
            ("load",   PRIM load),
            ("unquote", PRIM unquote'),
            ("unquote-splicing", PRIM unquoteSplicing'),            
            ("quasiquote", SYNT quasiquote')]


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


-- read-eval-print-loop
repl :: Env -> String -> IO ()
repl env xs = do
  putStr "Scm> "
  hFlush stdout
  case readSExpr xs of
    Left  (ParseErr xs' "EOF") -> return ()
    Left  (ParseErr xs' mes) -> do putStrLn mes
                                   repl env $ dropWhile (/= '\n') xs'
    Right (expr, xs') -> do let code = compile expr
                            let v = S.exec [] [] [] code [] 
                            putStrLn (show v)
                            repl env xs'

main :: IO ()
main = do
  xs <- hGetContents stdin
  ht <- H.fromList initGEnv
  repl (ht, []) xs



