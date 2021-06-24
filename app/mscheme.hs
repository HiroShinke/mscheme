--
-- mscheme.hs : microScheme
---
---              Copyright (C) 2013 Makoto Hiroi
---
{--

オリジナルは、以下で公開されたもの
「お気楽 Haskell プログラミング入門」
 http://www.nct9.ne.jp/m_hiroi/func/haskell.html

以下の点を変更しています。

・例外系はContorl.Monad.Trans.Exceptに移植
・HashTableは、hashtablesを使用
・unquote系の処理を、Haskell内部で処理。

--}

import Data.Char
import Data.IORef

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Data.HashTable.IO as H
import System.IO
import SExpr
import Error
import Reader
import Evaluator
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

-- read-eval-print-loop
repl :: Env -> String -> IO ()
repl env xs = do
  putStr "Scm> "
  hFlush stdout
  case readSExpr xs of
    Left  (ParseErr xs' "EOF") -> return ()
    Left  (ParseErr xs' mes) -> do putStrLn mes
                                   repl env $ dropWhile (/= '\n') xs'
    Right (expr, xs') -> do result <- runExceptT $ eval env expr 
                            case result of
                              Left e -> putStrLn (show e)
                              Right v  -> print v
                            repl env xs'

main :: IO ()
main = do
  xs <- hGetContents stdin
  ht <- H.fromList initGEnv
  repl (ht, []) xs

