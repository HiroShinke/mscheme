

module Primitives.Primitives where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Error
import SExpr

-- リスト操作
car :: PrimFunc
car NIL = throwE $ strMsg $ "car : " ++ errNEA
car (CELL (CELL a _) _) = return a
car _                   = throwE $ strMsg $ "car : " ++ errCELL

cdr :: PrimFunc
cdr NIL = throwE $ strMsg $ "cdr : " ++ errNEA
cdr (CELL (CELL _ d) _) = return d
cdr _                   = throwE $ strMsg $ "cdr : " ++ errCELL

cons :: PrimFunc
cons (CELL a (CELL b _)) = return (CELL a b)
cons _                   = throwE $ strMsg $ "cons : " ++ errNEA

pair :: PrimFunc
pair NIL                 = throwE $ strMsg $ "pair? : " ++ errNEA
pair (CELL (CELL _ _) _) = return true
pair _                   = return false

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

adds :: PrimFunc
adds xs = foldCell add (INT 0) xs

sub :: SExpr -> SExpr -> Scm SExpr
sub (INT x)  (INT y)  = return (INT (x - y))
sub (INT x)  (REAL y) = return (REAL (fromIntegral x - y))
sub (REAL x) (INT y)  = return (REAL (x - fromIntegral y))
sub (REAL x) (REAL y) = return (REAL (x - y))
sub _        _        = throwE $ strMsg $ "- : " ++ errNUM

subs :: PrimFunc
subs NIL = throwE $ strMsg $ "- : " ++ errNEA
subs (CELL (INT a) NIL)  = return (INT (-a))
subs (CELL (REAL a) NIL) = return (REAL (-a))
subs (CELL a rest) = foldCell sub a rest

mul :: SExpr -> SExpr -> Scm SExpr
mul (INT x)  (INT y)  = return (INT (x * y))
mul (INT x)  (REAL y) = return (REAL (fromIntegral x * y))
mul (REAL x) (INT y)  = return (REAL (x * fromIntegral y))
mul (REAL x) (REAL y) = return (REAL (x * y))
mul _        _        = throwE $ strMsg $ "- : " ++ errNUM

muls :: PrimFunc
muls xs = foldCell mul (INT 1) xs

div' :: SExpr -> SExpr -> Scm SExpr
div' _        (INT 0)  = throwE $ strMsg errZERO
div' _        (REAL 0) = throwE $ strMsg errZERO
div' (INT x)  (INT y)  = return (INT (x `div` y))
div' (INT x)  (REAL y) = return (REAL (fromIntegral x / y))
div' (REAL x) (INT y)  = return (REAL (x / fromIntegral y))
div' (REAL x) (REAL y) = return (REAL (x / y))
div' _        _        = throwE $ strMsg $ "- : " ++ errNUM

divs :: PrimFunc
divs NIL = throwE $ strMsg $ "/ : " ++ errNEA
divs (CELL a NIL)  = div' (INT 1) a
divs (CELL a rest) = foldCell div' a rest

mod' :: PrimFunc
mod' NIL          = throwE $ strMsg $ "mod : " ++ errNEA
mod' (CELL _ NIL) = throwE $ strMsg $ "mod : " ++ errNEA
mod' (CELL _ (CELL (INT 0) _))  = throwE $ strMsg errZERO
mod' (CELL _ (CELL (REAL 0) _)) = throwE $ strMsg errZERO
mod' (CELL (INT x) (CELL (INT y) _)) = return (INT (mod x y))
mod' _ = throwE $ strMsg $ "mod : " ++ errINT

-- 等値の判定
eq' :: PrimFunc
eq' (CELL x (CELL y _)) =
  if x == y then return true else return false
eq' _ = throwE $ strMsg $ "eq : " ++ errNEA

equal' :: PrimFunc
equal' (CELL x (CELL y _)) =
  if iter x y then return true else return false
  where iter (CELL a b) (CELL c d) = iter a c && iter b d
        iter x y = x == y
equal' _ = throwE $ strMsg $ "equal : " ++ errNEA

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

eqNum, ltNum, gtNum, ltEq, gtEq :: PrimFunc
eqNum = compareNums (== EQ)
ltNum = compareNums (== LT)
gtNum = compareNums (== GT)
ltEq  = compareNums (<= EQ)
gtEq  = compareNums (>= EQ)


-- list

list' :: PrimFunc
list' e = return e

-- error

error' :: PrimFunc
error' (CELL (STR x) NIL) = throwE $ strMsg $ "ERROR: " ++ x
error' (CELL (STR x) (CELL y _)) = throwE $ strMsg $ "ERROR: " ++ x ++ " " ++ show y
error' (CELL x _) = throwE $ strMsg $ "ERROR: " ++ show x
error' _ = throwE $ strMsg "ERROR: "


-- append

append' :: PrimFunc
append' (CELL (CELL h t) (CELL xs NIL))  = do
  t' <- append' (CELL t (CELL xs NIL))
  return (CELL h t')
append' (CELL NIL (CELL xs NIL))  = return xs
append' (CELL x xs)  = do
  liftIO $ putStrLn $ "append'3 x=" ++ (show x)
  liftIO $ putStrLn $ "appped'3 xs=" ++ (show xs)  
  throwE $ strMsg $ "append for invlid list!: " ++ (show x)

