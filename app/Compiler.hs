

module Compiler where

import Data.IORef
import qualified Secd as S
import Control.Monad.Trans.Except
import System.IO
import Error

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

type ScmFunc = Env -> SExpr -> Scm SExpr

-- S 式の定義
data SExpr = INT  !Integer
           | REAL !Double
           | SYM  String
           | STR  String
           | BOOL Bool
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
  BOOL x  == BOOL y  = x == y
  NIL    == NIL    = True
  _      == _      = False

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
  show (CELL (SYM "quote") (CELL e NIL)) = "'" ++ (show e)
  show (CELL (SYM "quasiquote") (CELL e NIL)) = "`" ++ (show e)
  show (CELL (SYM "unquote") (CELL e NIL)) = "," ++ (show e)
  show (CELL (SYM "unquote-splicing") (CELL e NIL)) = ",@" ++ (show e)
  show xs         = "(" ++ showCell xs ++ ")"


quote           = SYM "quote"
quasiquote      = SYM "quasiquote"
unquote         = SYM "unquote"
unquoteSplicing = SYM "unquote-splicing"

-- Scmエラーの定義
data ScmError = ScmError String String deriving Show

instance Error ScmError where
  noMsg    = ScmError "" ""
  strMsg s = ScmError  "" s

-- 評価器の定義
type Scm a = ExceptT ScmError IO a

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
comp env (BOOL n)   cs = S.Ldc (S.VBool n) : cs
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
  let code = compBody (sExpToValue args:env) body [S.Rtn]
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
    findCol :: S.Value -> Int -> Maybe Int
    findCol (S.Cell (S.Sym m) xs) j = if n == m then return j else findCol xs (j+1)
    findCol S.Nil _ = fail "not found"

sExpToValue :: SExpr -> S.Value
sExpToValue NIL        = S.Nil
sExpToValue (INT n)    = S.Num (fromIntegral n)
sExpToValue (STR n)    = S.Str n
sExpToValue (BOOL n)   = S.VBool n
sExpToValue (SYM name) = S.Sym name
sExpToValue (CELL x xy) = S.Cell (sExpToValue x) (sExpToValue xy)

cellToEnv :: SExpr -> [S.Value]
cellToEnv (CELL (SYM n) rest) = S.Sym n : cellToEnv rest
cellToEnv NIL = []

sExpLength :: SExpr -> Int
sExpLength (CELL h t) = 1 + sExpLength t
sExpLength NIL = 0
