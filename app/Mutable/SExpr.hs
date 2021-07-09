{-# LANGUAGE FlexibleInstances #-}

module Mutable.SExpr where

import Data.IORef
import System.IO.Unsafe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Error

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

showIORef :: Show a => IORef a -> String
showIORef x = unsafePerformIO $ do
  x' <- readIORef x
  return $ show x'

contentIORef :: IORef a -> a
contentIORef x = unsafePerformIO $ readIORef x

equalIORef :: Eq a => IORef a -> IORef a -> Bool
equalIORef x y = unsafePerformIO $ (==) <$> readIORef x <*> readIORef y
  

-- S 式の定義
data SExpr = INT  !Integer
           | REAL !Double
           | SYM  String
           | STR  String
           | BOOL Bool
           | CELL (IORef SExpr) (IORef SExpr)
           | NIL
           | PRIM ScmFunc
           | SYNT ScmFunc
           | CLOS SExpr LEnv
           | MACR SExpr
           | PRIM' SecdFunc       --- VM built-in function
           | CLOS' [Code] Frame   --- compiled closure
           | MACR' [Code] Frame   --- compiled macro
           | CONT Cont [Cont]

-- 等値の定義
instance Eq SExpr where
  INT x  == INT y  = x == y
  REAL x == REAL y = x == y
  SYM x  == SYM y  = x == y
  STR x  == STR y  = x == y
  BOOL x  == BOOL y  = x == y
  NIL    == NIL    = True
  CELL x xs == CELL y ys = x `equalIORef` y && xs `equalIORef` ys
  _      == _      = False

equalExpr :: SExpr -> SExpr -> IO Bool
equalExpr (INT x) (INT y) = return $ x == y
equalExpr (REAL x) (REAL y) = return $ x == y
equalExpr (SYM x) (SYM y) = return $ x == y
equalExpr (STR x) (STR y) = return $ x == y
equalExpr (BOOL x) (BOOL y) = return $ x == y
equalExpr NIL NIL = return True
equalExpr (CELL x xs) (CELL y ys) =
  (&&) <$> equalExprM x y <*> equalExprM xs ys
  where
    equalExprM x y = do
      x' <- readIORef x
      y' <- readIORef y
      equalExpr x' y'
equalExpr  _  _      = return False


instance Show SExpr where
  show (INT x)    = show x
  show (REAL x)   = show x
  show (SYM x)    = show x
  show (STR x)    = show x
  show (BOOL b)   = show b
  show  NIL       = "()"
  show (SYNT _)   = "<syntax>"
  show (PRIM _)   = "<primitive>"
  show (PRIM' _)  = "<primitive'>"
  show (CLOS _ _) = "<closure>"
  show xs         =
    let xs' = showCell xs
    in "(" ++ xs' ++ ")"

instance Show a => Show (IORef a) where
  show ma = showIORef ma

--
-- S 式の表示
--
showCell :: SExpr -> String
showCell (CELL a d) =
  show a ++ case (contentIORef d) of
              NIL      -> ""
              PRIM _   -> "<primitive>"
              PRIM' _  -> "<primitive'>"
              CLOS _ _ -> "<closure>"
              SYNT _   -> "<syntax>"
              INT x    -> " . " ++ show x
              REAL x   -> " . " ++ show x
              SYM x    -> " . " ++ x
              STR x    -> " . " ++ show x
              _        -> " " ++ showCell (contentIORef d)
showCell xs = show xs

showCellIO (CELL ma md) = do
  a <- readIORef ma
  d <- readIORef md
  (++) <$> showIO a  <*> case d of
                           NIL      -> return ""
                           PRIM _   -> return "<primitive>"
                           PRIM' _  -> return "<primitive'>"
                           CLOS _ _ -> return "<closure>"
                           SYNT _   -> return "<syntax>"
                           INT x    -> return $ " . " ++ show x
                           REAL x   -> return $ " . " ++ show x
                           SYM x    -> return $ " . " ++ x
                           STR x    -> return $ " . " ++ show x
                           _        -> (++) <$> return " " <*> showCellIO d
showCellIO xs = showIO xs


showIO (INT x)    = return $ show x
showIO (REAL x)   = return $ show x
showIO (SYM x)    = return x
showIO (STR x)    = return $ show x
showIO (BOOL b)   = return $ show b
showIO NIL        = return "()"
showIO (SYNT _)   = return "<syntax>"
showIO (PRIM _)   = return "<primitive>"
showIO (PRIM' _)  = return "<primitive'>"
showIO (CLOS _ _) = return "<closure>"
--showIO (CELL (SYM "quote") (CELL e es)) = "'" ++ (show e)
--showIO (CELL (SYM "quasiquote") (CELL e es)) = "`" ++ (show e)
--showIO (CELL (SYM "unquote") (CELL e es)) = "," ++ (show e)
--showIO (CELL (SYM "unquote-splicing") (CELL e es)) = ",@" ++ (show e)
showIO xs         = do
  xs' <- showCellIO xs
  return $ "(" ++ xs' ++ ")"


cons :: SExpr -> SExpr -> IO SExpr
cons x xs = CELL <$> newIORef x <*> newIORef xs
  

-- Scmエラーの定義
data ScmError = ScmError String String deriving (Show, Eq)

instance Error ScmError where
  noMsg    = ScmError "" ""
  strMsg s = ScmError  "" s

type Scm a    = ExceptT ScmError IO a
type ScmFunc  = Env  -> SExpr -> Scm SExpr
type SecdFunc = GEnv -> SExpr -> Scm SExpr

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
type Frame = [IORef SExpr]
type Stack = [SExpr]
type Dump  = [Cont]

type Env'  = (GEnv,Frame)

data Cont = Cont3 Stack Frame [Code]
          | Cont1 [Code]
  deriving Show

data Code = Ld (Int,Int)
          | Ldc SExpr
          | Ldg String
          | Ldf [Code]
          | Ldct [Code]
          | Args Int
          | ArgsAp Int
          | App
          | TApp
          | Rtn
          | Sel [Code] [Code]
          | Selr [Code] [Code]
          | Join
          | Pop
          | Def String
          | Defm String
          | Stop
          | Dump
  deriving Show


-- 真偽値
true  = BOOL True
false = BOOL False

debugPrintOn = False

debugPrint :: String -> Scm ()
debugPrint msg = if debugPrintOn
                 then liftIO $ putStrLn $ msg
                 else return ()

type CompilerProc   = Env' -> SExpr -> [Code] -> Bool -> Scm [Code]
type CompilerProc'  = Env' -> SExpr -> [Code] -> Scm [Code]
type TranslatorProc = Env' -> SExpr -> [Code] -> Scm [Code]


