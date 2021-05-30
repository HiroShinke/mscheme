module Main where

import Lib
import qualified Data.HashTable.IO as H
import Data.IORef
import Control.Monad.Trans.Except

type HashTable k v = H.CuckooHashTable k v

-- S 式の定義
data SExpr = INT  !Integer
           | REAL !Double
           | SYM  String
           | STR  String
           | CELL SExpr SExpr
           | NIL
           | PRIM (SExpr -> Scm SExpr)
           | SYNT (Env -> SExpr -> Scm SExpr)
           | CLOS SExpr LEnv

-- 等値の定義
instance Eq SExpr where
  INT x  == INT y  = x == y
  REAL x == REAL y = x == y
  SYM x  == SYM y  = x == y
  STR x  == STR y  = x == y
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
  show xs         = "(" ++ showCell xs ++ ")"

foo :: IO (HashTable Int Int)
foo = do
    ht <- H.new
    H.insert ht 1 1
    H.insert ht 2 1
    H.insert ht 3 1    
    return ht

main :: IO ()
main = do
  ht <- foo
  l  <- H.toList ht
  print (show l)
  
-- ローカル環境の定義
type LEnv = [(String, IORef SExpr)]

-- グローバルな環境
type GEnv = HashTable String SExpr

-- 両方の環境を保持する
type Env = (GEnv, LEnv)

-- 評価器の定義
type Scm a = ExceptT String IO a
