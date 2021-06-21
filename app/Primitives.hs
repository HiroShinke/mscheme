

module Primitives where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Error
import Compiler

-- 真偽値
true  = SYM "true"
false = SYM "false"

-- Primitive の定義
errNUM  = "Illegal argument, Number required"
errINT  = "Illegal argument, Integer required"
errNEA  = "Not enough arguments"
errCELL = "Illegal argument, List required"
errZERO = "Divide by zero"


-- リスト操作
car :: ScmFunc
car _ NIL = throwE $ strMsg $ "car : " ++ errNEA
car _ (CELL (CELL a _) _) = return a
car _ _                   = throwE $ strMsg $ "car : " ++ errCELL

cdr :: ScmFunc
cdr _ NIL = throwE $ strMsg $ "cdr : " ++ errNEA
cdr _ (CELL (CELL _ d) _) = return d
cdr _ _                   = throwE $ strMsg $ "cdr : " ++ errCELL

cons :: ScmFunc
cons _ (CELL a (CELL b _)) = return (CELL a b)
cons _ _                   = throwE $ strMsg $ "cons : " ++ errNEA

pair :: ScmFunc
pair _ NIL                 = throwE $ strMsg $ "pair? : " ++ errNEA
pair _ (CELL (CELL _ _) _) = return true
pair _ _                   = return false

-- 畳み込み
foldCell :: (SExpr -> SExpr -> Scm SExpr) -> SExpr -> SExpr -> Scm SExpr
foldCell _ a NIL = return a
foldCell f a (CELL x rest) = do v <- f a x
                                foldCell f v rest
foldCell _ _ _ = throwE $ strMsg $ errCELL

-- 四則演算
add :: SExpr -> SExpr -> Scm SExpr
add (INT x)  (INT y)  = return (INT (x + y))
add (INT x)  (REAL y) = return (REAL (fromIntegral x + y))
add (REAL x) (INT y)  = return (REAL (x + fromIntegral y))
add (REAL x) (REAL y) = return (REAL (x + y))
add _        _        = throwE $ strMsg $ "+ : " ++ errNUM

adds :: ScmFunc
adds _ xs = foldCell add (INT 0) xs

sub :: SExpr -> SExpr -> Scm SExpr
sub (INT x)  (INT y)  = return (INT (x - y))
sub (INT x)  (REAL y) = return (REAL (fromIntegral x - y))
sub (REAL x) (INT y)  = return (REAL (x - fromIntegral y))
sub (REAL x) (REAL y) = return (REAL (x - y))
sub _        _        = throwE $ strMsg $ "- : " ++ errNUM

subs :: ScmFunc
subs _ NIL = throwE $ strMsg $ "- : " ++ errNEA
subs _ (CELL (INT a) NIL)  = return (INT (-a))
subs _ (CELL (REAL a) NIL) = return (REAL (-a))
subs _ (CELL a rest) = foldCell sub a rest

mul :: SExpr -> SExpr -> Scm SExpr
mul (INT x)  (INT y)  = return (INT (x * y))
mul (INT x)  (REAL y) = return (REAL (fromIntegral x * y))
mul (REAL x) (INT y)  = return (REAL (x * fromIntegral y))
mul (REAL x) (REAL y) = return (REAL (x * y))
mul _        _        = throwE $ strMsg $ "- : " ++ errNUM

muls :: ScmFunc
muls _ xs = foldCell mul (INT 1) xs

div' :: SExpr -> SExpr -> Scm SExpr
div' _        (INT 0)  = throwE $ strMsg errZERO
div' _        (REAL 0) = throwE $ strMsg errZERO
div' (INT x)  (INT y)  = return (INT (x `div` y))
div' (INT x)  (REAL y) = return (REAL (fromIntegral x / y))
div' (REAL x) (INT y)  = return (REAL (x / fromIntegral y))
div' (REAL x) (REAL y) = return (REAL (x / y))
div' _        _        = throwE $ strMsg $ "- : " ++ errNUM

divs :: ScmFunc
divs _ NIL = throwE $ strMsg $ "/ : " ++ errNEA
divs _ (CELL a NIL)  = div' (INT 1) a
divs _ (CELL a rest) = foldCell div' a rest

mod' :: ScmFunc
mod' _ NIL          = throwE $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ NIL) = throwE $ strMsg $ "mod : " ++ errNEA
mod' _ (CELL _ (CELL (INT 0) _))  = throwE $ strMsg errZERO
mod' _ (CELL _ (CELL (REAL 0) _)) = throwE $ strMsg errZERO
mod' _ (CELL (INT x) (CELL (INT y) _)) = return (INT (mod x y))
mod' _ _ = throwE $ strMsg $ "mod : " ++ errINT

-- 等値の判定
eq' :: ScmFunc
eq' _ (CELL x (CELL y _)) =
  if x == y then return true else return false
eq' _ _ = throwE $ strMsg $ "eq : " ++ errNEA

equal' :: ScmFunc
equal' _ (CELL x (CELL y _)) =
  if iter x y then return true else return false
  where iter (CELL a b) (CELL c d) = iter a c && iter b d
        iter x y = x == y
equal' _ _ = throwE $ strMsg $ "equal : " ++ errNEA

-- 数値の比較演算子
compareNum :: SExpr -> SExpr -> Scm Ordering
compareNum (INT x)  (INT y)  = return $ compare x y
compareNum (INT x)  (REAL y) = return $ compare (fromIntegral x) y
compareNum (REAL x) (INT y)  = return $ compare x (fromIntegral y)
compareNum (REAL x) (REAL y) = return $ compare x y
compareNum _ _ = throwE $ strMsg errNUM

compareNums :: (Ordering -> Bool) -> SExpr -> Scm SExpr
compareNums _ NIL          = throwE $ strMsg errNEA
compareNums _ (CELL _ NIL) = throwE $ strMsg errNEA
compareNums p (CELL x (CELL y NIL)) = do
  r <- compareNum x y
  if p r then return true else return false
compareNums p (CELL x ys@(CELL y _)) = do
  r <- compareNum x y
  if p r then compareNums p ys else return false
compareNums _ _ = throwE $ strMsg "invalid function form"

eqNum, ltNum, gtNum, ltEq, gtEq :: ScmFunc
eqNum _ = compareNums (== EQ)
ltNum _ = compareNums (== LT)
gtNum _ = compareNums (== GT)
ltEq  _ = compareNums (<= EQ)
gtEq  _ = compareNums (>= EQ)

-- list

list' :: ScmFunc
list' _  e = return e

-- append

append' :: ScmFunc
append' e  (CELL (CELL h t) (CELL xs NIL))  = do
  t' <- append' e (CELL t (CELL xs NIL))
  return (CELL h t')
append' e  (CELL NIL (CELL xs NIL))  = return xs
append' e  (CELL x xs)  = do
  liftIO $ putStrLn $ "append'3 x=" ++ (show x)
  liftIO $ putStrLn $ "appped'3 xs=" ++ (show xs)  
  throwE $ strMsg $ "append for invlid list!: " ++ (show x)

-- error

error' :: ScmFunc
error' _ (CELL (STR x) NIL) = throwE $ strMsg $ "ERROR: " ++ x
error' _ (CELL (STR x) (CELL y _)) = throwE $ strMsg $ "ERROR: " ++ x ++ " " ++ show y
error' _ (CELL x _) = throwE $ strMsg $ "ERROR: " ++ show x
error' _ _ = throwE $ strMsg "ERROR: "

