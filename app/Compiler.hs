

module Compiler where

import Data.IORef
import qualified Secd as S
import Control.Monad.Trans.Except
import SExpr
import System.IO
import Error

--- compile

compile :: SExpr -> [Code]
compile expr = comp [] expr [Stop]

comp :: Frame -> SExpr -> [Code] -> [Code]
comp env v@NIL        cs = Ldc v : cs
comp env v@(INT n)    cs = Ldc v : cs
comp env v@(STR n)    cs = Ldc v : cs
comp env v@(BOOL n)   cs = Ldc v : cs
comp env v@(SYM name) cs = let pos = findPos name env
                         in
                           case pos of
                             Just (i,j) -> Ld (i,j) :cs
                             Nothing    -> Ldg name : cs
comp env (CELL (SYM "quote") (CELL e NIL) ) cs  = Ldc e : cs
comp env (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) cs =
  let tc = comp env tb [Join]
      ec = comp env eb [Join]
  in
    comp env pred (Sel tc ec : cs)
comp env (CELL (SYM "if") (CELL pred (CELL tb NIL ))) cs =
  let tc = comp env tb [Join]
      ec = [Ldc NIL, Join]
  in
    comp env pred (Sel tc ec : cs)
comp env (CELL (SYM "lambda") (CELL args body)) cs = 
  let code = compBody (args:env) body [Rtn]
  in Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Def n : cs)
comp env (CELL (SYM "define-macro") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Defm n : cs)

comp env (CELL func args) cs =
  let cs' = comp env func (App:cs)
  in
    compArguments env args (Args (sExpLength args):cs')
      
compArguments env (CELL a as) cs = comp env a (compArguments env as cs)
compArguments _  NIL cs = cs

compBody env (CELL e NIL) cs = comp env e cs
compBody env (CELL e es) cs  = comp env e (Pop : compBody env es cs)


findPos :: String -> Frame -> Maybe (Int,Int)
findPos n xs = findFrame xs 0
  where
    findFrame :: Frame -> Int -> Maybe (Int,Int)
    findFrame (f:fs) i = case (findCol f 0) of
                         Just j -> return (i,j)
                         Nothing -> findFrame fs (i + 1)
    findFrame [] _  =   fail "not found"
    findCol :: SExpr -> Int -> Maybe Int
    findCol (CELL (SYM m) xs) j = if n == m then return j else findCol xs (j+1)
    findCol NIL _ = fail "not found"


cellToEnv :: SExpr -> [SExpr]
cellToEnv (CELL (SYM n) rest) = SYM n : cellToEnv rest
cellToEnv NIL = []

sExpLength :: SExpr -> Int
sExpLength (CELL h t) = 1 + sExpLength t
sExpLength NIL = 0
