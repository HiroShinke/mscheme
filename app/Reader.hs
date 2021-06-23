

module Reader where

import Data.Char

import Evaluator
import Compiler
import Error
import SExpr

-- パーサエラーの定義
data ParseErr = ParseErr String String deriving Show

instance Error ParseErr where
  noMsg    = ParseErr "" ""
  strMsg s = ParseErr "" s

type Parser a = Either ParseErr a

--
-- S 式の読み込み
--

isAlpha' :: Char -> Bool
isAlpha' x = elem x "!$%&*+-/:<=>?@^_~"

isIdent0 :: Char -> Bool
isIdent0 x = isAlpha x || isAlpha' x

isIdent1 :: Char -> Bool
isIdent1 x = isAlphaNum x || isAlpha' x

isREAL :: Char -> Bool
isREAL x = elem x ".eE"

isNUM :: String -> Bool
isNUM (x:_) = isDigit x
isNUM _     = False

getNumber :: String -> Parser (SExpr, String)
getNumber xs =
  let (s, ys) = span isDigit xs
  in if not (null ys) && isREAL (head ys)
     then case reads xs of
            [] -> Left noMsg  -- ありえないエラー
            [(y', ys')] -> return (REAL y', ys')
     else return (INT (read s), ys)

readSExpr :: String -> Parser (SExpr, String)
readSExpr [] = Left $ strMsg "EOF"
readSExpr (x:xs) 
  | isSpace x  = readSExpr xs
  | isDigit x  = getNumber (x:xs)
  | isIdent0 x = if x == '+' && isNUM xs
                 then getNumber xs
                 else if x == '-' && isNUM xs
                 then do (y, ys) <- getNumber xs
                         case y of
                           INT x  -> return (INT  (- x), ys)
                           REAL x -> return (REAL (- x), ys)
                 else let (name, ys) = span isIdent1 (x:xs)
                      in return (SYM name, ys)
  | otherwise  =
      case x of
        '('  -> readCell 0 xs
        ';'  -> readSExpr $ dropWhile (/= '\n') xs
        '"'  -> case reads (x:xs) of
                  [] -> Left noMsg
                  [(y, ys)] -> return (STR y, ys)
        '\'' -> readSExpr xs >>= \(e, ys) -> return (CELL quote (CELL e NIL), ys)
        '`'  -> readSExpr xs >>= \(e, ys) -> return (CELL quasiquote (CELL e NIL), ys)
        ','  -> if not (null xs) && head xs == '@'
                  then readSExpr (tail xs) >>=
                       \(e, ys) -> return (CELL unquoteSplicing (CELL e NIL), ys)
                  else readSExpr xs >>=
                       \(e, ys) -> return (CELL unquote (CELL e NIL), ys)
        _    -> Left $ ParseErr xs ("unexpected token: " ++ show x)

readCell :: Int -> String -> Parser (SExpr, String)
readCell _ [] = Left $ strMsg "EOF"
readCell n (x:xs)
  | isSpace x = readCell n xs
  | otherwise =
      case x of
        ')' -> return (NIL, xs)
        '.' -> if n == 0
               then Left $ ParseErr xs "invalid dotted list"
               else do (e, ys) <- readSExpr xs
                       case dropWhile isSpace ys of
                         ')':zs -> return (e, zs)
                         _      -> Left $ ParseErr xs "invalid dotted list"
        '(' -> do (a, ys) <- readCell 0 xs
                  (d, zs) <- readCell 1 ys
                  return (CELL a d, zs)
        _   -> do (a, ys) <- readSExpr (x:xs)
                  (d, zs) <- readCell 1 ys
                  return (CELL a d, zs)

