

module SExpr where

import Data.IORef
import Control.Monad.Trans.Except
import Error

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

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
           | CLOS' [Code] Frame
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


-- Scmエラーの定義
data ScmError = ScmError String String deriving Show

instance Error ScmError where
  noMsg    = ScmError "" ""
  strMsg s = ScmError  "" s

type Scm a = ExceptT ScmError IO a
type ScmFunc = Env -> SExpr -> Scm SExpr


quote           = SYM "quote"
quasiquote      = SYM "quasiquote"
unquote         = SYM "unquote"
unquoteSplicing = SYM "unquote-splicing"


-- ローカル環境の定義(インタプリタ用)
type LEnv = [(String, IORef SExpr)]
-- グローバルな環境
type GEnv = HashTable String SExpr
-- 両方の環境を保持する
type Env = (GEnv, LEnv)

-- stack of frames. a frame must be list of symbol  
type Frame = [SExpr] 
type Stack = [SExpr]
type Dump  = [Cont]

data Cont = Cont3 Stack Frame [Code]
          | Cont1 [Code]
  deriving Show

data Code = Ld (Int,Int)
          | Ldc SExpr
          | Ldg String
          | Ldf [Code]
          | Args Int
          | App
          | Rtn
          | Sel [Code] [Code]
          | Join
          | Pop
          | Def String
          | Stop
          | Dump
  deriving (Show,Eq)
