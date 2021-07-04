

module Compiler where

import Data.IORef
import qualified Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import SExpr
import System.IO
import Error
import qualified SecdFuncs as F
import Translator

--- compile

compile :: GEnv -> SExpr -> Scm [Code]
compile g expr = comp (g,[]) expr [Stop] False

comp :: CompilerProc
comp env v@NIL        cs tail = return $ Ldc v : cs
comp env v@(INT n)    cs tail = return $ Ldc v : cs
comp env v@(STR n)    cs tail = return $ Ldc v : cs
comp env v@(BOOL n)   cs tail = return $ Ldc v : cs
comp (g,e) v@(SYM name) cs tail = let pos = findPos name e
                             in do
  debugPrint $ "comp sym: sym=" ++ name
  debugPrint $ "comp sym: e=" ++ (show e)
  case pos of
    Just (i,j) -> return $ Ld (i,j) :cs
    Nothing    -> return $ Ldg name : cs
comp env (CELL (SYM "quote") (CELL e NIL) ) cs  tail = return $ Ldc e : cs
comp env (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) cs tail = if tail
  then do
  tc <- comp env tb [Rtn] True
  ec <- comp env eb [Rtn] True
  comp env pred (Selr tc ec : cs) False
  else do
  tc <- comp env tb [Join] False
  ec <- comp env eb [Join] False
  comp env pred (Sel tc ec : cs) False
  
comp env (CELL (SYM "if") (CELL pred (CELL tb NIL ))) cs tail = if tail
  then do
  tc <- comp env tb [Rtn] True
  let ec = [Ldc NIL, Rtn]
  comp env pred (Sel tc ec : cs) False
  else do
  tc <- comp env tb [Join] False
  let ec = [Ldc NIL, Join]
  comp env pred (Sel tc ec : cs) False
  
comp (g,e) (CELL (SYM "lambda") (CELL args body)) cs tail = do
  debugPrint $ "comp lambda: args=" ++ (show args)
  debugPrint $ "comp lambda: body=" ++ (show body)  
  code <- compBody (g,(args:e)) body [Rtn]
  return $ Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs tail =
  comp env e (Def n : cs) False
comp env (CELL (SYM "define-macro") (CELL (SYM n) (CELL e NIL))) cs tail =
  comp env e (Defm n : cs) False
comp env (CELL (SYM "quasiquote") (CELL e NIL) ) cs tail =
  translator 0 comp env e cs
comp env (CELL (SYM "unquote") (CELL e NIL) ) cs tail =
  throwE $ strMsg "unquote appeared outside quasiquote"
comp env (CELL (SYM "unquote-splicing") (CELL e NIL) ) cs tail =
  throwE $ strMsg "unquote-splicing appeared outside quasiquote"
comp env (CELL (SYM "call/cc") (CELL e NIL)) cs tail = do
  cs' <- comp env e (App:cs) False
  return $ [Ldct cs, Args 1] ++ cs'
comp env (CELL (SYM "apply") (CELL func args)) cs tail = do
  cs' <- comp env func (App:cs) False
  compArguments env args (ArgsAp (sExpLength args):cs')
  
comp env@(g,e) (CELL func@(SYM sym) args) cs tail = do
  x <- liftIO $ H.lookup g sym
  case x of
    Just (MACR' code e) -> do
      debugPrint $ "apply macro: sym=" ++ sym
      debugPrint $ "apply macro: code=" ++ (show code)
      debugPrint $ "apply macro: e=" ++ (show e)
      debugPrint $ "apply macro: args="  ++ (show args)
      args' <- S.exec g [] (args:e) code [Cont3 [] [] [Stop]]
      debugPrint $ "apply macro: args'="  ++ (show args')
      comp env args' cs False
    Just _ -> comp' env (CELL func args) cs tail
    Nothing -> comp' env (CELL func args) cs tail

comp env (CELL func args) cs tail = comp' env (CELL func args) cs tail

comp' env (CELL func args) cs tail = do
  cs' <- comp env func (if tail then TApp:cs else App:cs) False
  compArguments env args (Args (sExpLength args):cs')

compArguments :: CompilerProc'
compArguments env (CELL a as) cs = do
  cs' <- compArguments env as cs
  comp env a cs' False
compArguments _  NIL cs = return $ cs

compBody :: CompilerProc'
compBody env (CELL e NIL) cs = comp env e cs True
compBody env (CELL e es) cs = do
  cs' <- compBody env es cs
  comp env e (Pop : cs') False


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
    findCol (SYM m) j = if n == m then return (-(j+1)) else fail "not found"
    findCol NIL _ = fail "not found"

sExpLength :: SExpr -> Int
sExpLength (CELL h t) = 1 + sExpLength t
sExpLength NIL = 0

