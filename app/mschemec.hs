--
-- MSchemec.hs : microScheme compiler
---

import Data.Char

import System.IO
import qualified Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except

import Reader
import Compiler
import Error
import SExpr
import SecdFuncs

--
-- 大域変数の初期化
--
initGEnv :: [(String, SExpr)]
initGEnv = [("true",   true),
            ("false",  false),
            ("eq?",    PRIM' eq'),
            ("equal?", PRIM' equal'),
            ("pair?",  PRIM' pair),
            ("+",      PRIM' adds),
            ("-",      PRIM' subs),
            ("*",      PRIM' muls),
            ("/",      PRIM' divs),
            ("mod",    PRIM' mod'),
            ("=",      PRIM' eqNum),
            ("<",      PRIM' ltNum),
            (">",      PRIM' gtNum),
            ("<=",     PRIM' ltEq),
            (">=",     PRIM' gtEq),
            ("car",    PRIM' car),
            ("cdr",    PRIM' cdr),
            ("cons",   PRIM' cons),
            ("list",   PRIM' list'),
            ("append", PRIM' append'),
            ("error",  PRIM' error'),
            ("load",   PRIM' load)
           ]


-- read-eval-print-loop
repl :: GEnv -> String -> IO ()
repl g xs = do
  putStr "Scm> "
  hFlush stdout
  case readSExpr xs of
    Left  (ParseErr xs' "EOF") -> return ()
    Left  (ParseErr xs' mes) -> do putStrLn mes
                                   repl g $ dropWhile (/= '\n') xs'
    Right (expr, xs') -> do code <- runExceptT $ compile g expr
                            putStrLn "Code:"
                            putStrLn (show code)
                            case code of
                              Right cd ->  do
                                v <- runExceptT $ S.exec g [] [] cd []
                                putStrLn (show v)
                              Left err -> do
                                putStrLn (show err)
                            repl g xs'
main :: IO ()
main = do
  xs <- hGetContents stdin
  ht <- H.fromList initGEnv
  repl ht xs



