

module Compiler where

import Data.IORef
import qualified Secd as S
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

-- グローバルな環境
type GEnv = HashTable String SExpr

-- 両方の環境を保持する
type Env = (GEnv, LEnv)

--- compile

compile :: SExpr -> [S.Code]
compile expr = comp [] expr [S.Stop]

comp :: S.Env -> SExpr -> [S.Code] -> [S.Code]
comp env NIL        cs = S.Ldc S.Nil : cs
comp env (INT n)    cs = S.Ldc (S.Num (fromIntegral n)) : cs
comp env (STR n)    cs = S.Ldc (S.Str n) : cs
comp env (SYM name) cs = let pos = findPos name env
                         in
                           case pos of
                             Just (i,j) -> S.Ld (i,j) :cs
                             Nothing    -> S.Ldg name : cs
comp env (CELL (SYM "quote") (CELL e NIL) ) cs  = S.Ldc (sExpToValue e) : cs
comp env (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) cs =
  let tc = comp env tb [S.Join]
      ec = comp env eb [S.Join]
  in
    comp env pred (S.Sel tc ec : cs)
comp env (CELL (SYM "if") (CELL pred (CELL tb NIL ))) cs =
  let tc = comp env tb [S.Join]
      ec = [S.Ldc S.Nil, S.Join]
  in
    comp env pred (S.Sel tc ec : cs)
comp env (CELL (SYM "lambda") (CELL args body)) cs = 
  let code = compBody (cellToEnv args:env) body [S.Rtn]
  in S.Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (S.Def n : cs)

comp env (CELL func args) cs =
  let cs' = comp env func (S.App:cs)
  in
    compArguments env args (S.Args (sExpLength args):cs')
      
compArguments env (CELL a as) cs = comp env a (compArguments env as cs)
compArguments _  NIL cs = cs

compBody env (CELL e NIL) cs = comp env e cs
compBody env (CELL e es) cs  = comp env e (S.Pop : compBody env es cs)


findPos :: String -> S.Env -> Maybe (Int,Int)
findPos n xs = findFrame xs 0
  where
    findFrame :: S.Env -> Int -> Maybe (Int,Int)
    findFrame (f:fs) i = case (findCol f 0) of
                         Just j -> return (i,j)
                         Nothing -> findFrame fs (i + 1)
    findFrame [] _  =   fail "not found"
    findCol :: [S.Value] -> Int -> Maybe Int
    findCol ((S.Sym m):xs) j = if n == m then return j else findCol xs (j+1)
    findCol [] _ = fail "not found"

sExpToValue :: SExpr -> S.Value
sExpToValue = error "not implemented yet"

cellToEnv :: SExpr -> [S.Value]
cellToEnv (CELL (SYM n) rest) = S.Sym n : cellToEnv rest
cellToEnv NIL = []

sExpLength :: SExpr -> Int
sExpLength (CELL h t) = 1 + sExpLength t
sExpLength NIL = 0
