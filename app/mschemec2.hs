--
-- MSchemec.hs : microScheme compiler
---

import Data.Char

import System.IO
import SExpr
import qualified Mutable.Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Reader
import Compiler
import Error
import qualified Mutable.SExpr as M
import qualified Mutable.SecdFuncs as F

--
-- 大域変数の初期化
--
initGEnv :: [(String, M.SExpr)]
initGEnv = [("true",   M.true),
            ("false",  M.false),
            ("eq?",    M.PRIM' F.eq'),
            ("equal?", M.PRIM' F.equal'),
            ("pair?",  M.PRIM' F.pair),
            ("+",      M.PRIM' F.adds),
            ("-",      M.PRIM' F.subs),
            ("*",      M.PRIM' F.muls),
            ("/",      M.PRIM' F.divs),
            ("mod",    M.PRIM' F.mod'),
            ("=",      M.PRIM' F.eqNum),
            ("<",      M.PRIM' F.ltNum),
            (">",      M.PRIM' F.gtNum),
            ("<=",     M.PRIM' F.ltEq),
            (">=",     M.PRIM' F.gtEq),
            ("car",    M.PRIM' F.car),
            ("cdr",    M.PRIM' F.cdr),
            ("cons",   M.PRIM' F.cons),
            ("list",   M.PRIM' F.list'),
            ("append", M.PRIM' F.append'),
            ("error",  M.PRIM' F.error')
           ]


load :: M.SecdFunc
load g args = do
  x <- liftIO $ S.mtoi args
  load' g x

load' g (CELL (STR filename) _ ) = do
  xs <- liftIO $ readFile filename
  r <- iter xs
  if r then return M.true else return M.false
  where
    iter :: String -> Scm Bool
    iter xs = 
      case readSExpr xs of
        Left  (ParseErr xs' "EOF") -> return True
        Left  (ParseErr xs' mes) -> do liftIO $ putStrLn mes
                                       return False
        Right (expr, xs') -> do g' <- liftIO $ H.new  --- macro is not supported yet.
                                code' <- compile g' expr
                                code  <-liftIO $  mapM S.itomCode code'
                                liftIO $ putStrLn "Code:"
                                liftIO $ putStrLn (show code)
                                v <- S.exec g [] [] code []
                                v' <- liftIO $ S.mtoi v
                                liftIO $ putStrLn (show v)
                                iter xs'
load' _ _ = throwE $ strMsg "invalid load form"

-- read-eval-print-loop
repl :: M.GEnv -> String -> IO ()
repl g xs = do
  g' <- H.new
  putStr "Scm> "
  hFlush stdout
  case readSExpr xs of
    Left  (ParseErr xs' "EOF") -> return ()
    Left  (ParseErr xs' mes) -> do putStrLn mes
                                   repl g $ dropWhile (/= '\n') xs'
    Right (expr, xs') -> do code <- runExceptT $ compile g' expr
                            putStrLn "Code:"
                            putStrLn (show code)
                            case code of
                              Right cd ->  do
                                cd' <- mapM S.itomCode cd
                                v <- runExceptT $ S.exec g [] [] cd' []
                                v' <- mapM S.mtoi v
                                putStrLn (show v')
                              Left err -> do
                                putStrLn (show err)
                            repl g xs'
main :: IO ()
main = do
  xs <- hGetContents stdin
  ht <- H.fromList initGEnv
  repl ht xs



