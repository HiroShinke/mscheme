--
-- MSchemec.hs : microScheme compiler
---

import Data.Char

import System.IO
import SExpr
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Reader
import LLVM.Compiler
import Error

import System.Environment (getArgs)
import System.FilePath.Posix (replaceExtension)
-- import qualified Data.Text.IO as T
-- import qualified Data.Text.Lazy.IO as LT

--
-- 大域変数の初期化
--

-- read-eval-print-loop
readSExprs :: String -> IO [SExpr]
readSExprs xs = do
  case readSExpr xs of
    Left  (ParseErr xs' "EOF") -> return []
    Left  (ParseErr xs' mes) -> do
      putStrLn mes
      return []
    Right (expr, xs') -> (:) <$> pure expr <*> readSExprs xs'

main :: IO ()
main = do
  args <- getArgs
  let srcPath = args !! 0
  let distPath = replaceExtension srcPath ".ll"
  src <- readFile srcPath
  es <- readSExprs src
  writeFile distPath (compile es)






