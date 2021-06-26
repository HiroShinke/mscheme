

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

--- compile

compile :: GEnv -> SExpr -> Scm [Code]
compile g expr = comp (g,[]) expr [Stop]

comp :: Env' -> SExpr -> [Code] -> Scm [Code]
comp env v@NIL        cs = return $ Ldc v : cs
comp env v@(INT n)    cs = return $ Ldc v : cs
comp env v@(STR n)    cs = return $ Ldc v : cs
comp env v@(BOOL n)   cs = return $ Ldc v : cs
comp (g,e) v@(SYM name) cs = let pos = findPos name e
                         in
                           case pos of
                             Just (i,j) -> return $ Ld (i,j) :cs
                             Nothing    -> return $ Ldg name : cs
comp env (CELL (SYM "quote") (CELL e NIL) ) cs  = return $ Ldc e : cs
comp env (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) cs = do
  tc <- comp env tb [Join]
  ec <- comp env eb [Join]
  comp env pred (Sel tc ec : cs)
comp env (CELL (SYM "if") (CELL pred (CELL tb NIL ))) cs = do
  tc <- comp env tb [Join]
  let ec = [Ldc NIL, Join]
  comp env pred (Sel tc ec : cs)
comp (g,e) (CELL (SYM "lambda") (CELL args body)) cs = do
  code <- compBody (g,(args:e)) body [Rtn]
  return $ Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Def n : cs)
comp env (CELL (SYM "define-macro") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Defm n : cs)

comp env@(g,e) (CELL func@(SYM sym) args) cs = do
  x <- liftIO $ H.lookup g sym
  case x of
    Just (MACR' code e) -> do
--    liftIO $ putStrLn $ "xxxxx: code=" ++ (show code)
--    liftIO $ putStrLn $ "xxxxx: e=" ++ (show e)
--    liftIO $ putStrLn $ "args: "  ++ (show args)
      args' <- S.exec g [] (args:e) code [Cont3 [] [] [Stop]]
--    liftIO $ putStrLn $ "args': "  ++ (show args')
      comp env args' cs
    Just _ -> comp' env (CELL func args) cs
    Nothing -> comp' env (CELL func args) cs

comp env (CELL func args) cs = comp' env (CELL func args) cs

comp' env (CELL func args) cs = do
  cs' <- comp env func (App:cs)
  compArguments env args (Args (sExpLength args):cs')

compArguments env (CELL a as) cs = do
  cs' <- compArguments env as cs
  comp env a cs'
compArguments _  NIL cs = return $ cs

compBody env (CELL e NIL) cs = comp env e cs
compBody env (CELL e es) cs  = do
  cs' <- compBody env es cs
  comp env e (Pop : cs')


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
