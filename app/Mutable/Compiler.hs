

module Mutable.Compiler where

import Data.IORef
import qualified Mutable.Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import SExpr
import qualified Mutable.SExpr as M
import System.IO
import Error
import qualified SecdFuncs as F
import Mutable.Translator

--- compile

compile :: M.GEnv -> SExpr -> Scm [M.Code]
compile g expr = comp (g,[]) expr [M.Stop] False

comp :: M.CompilerProc
comp env v@NIL        cs tail = return $ M.Ldc M.NIL : cs
comp env v@(INT n)    cs tail = return $ M.Ldc (M.INT n) : cs
comp env v@(STR n)    cs tail = return $ M.Ldc (M.STR n) : cs
comp env v@(BOOL n)   cs tail = return $ M.Ldc (M.BOOL n) : cs 
comp (g,e) v@(SYM name) cs tail = do
  pos <- liftIO $ findPos name e
  debugPrint $ "comp sym: sym=" ++ name
  debugPrint $ "comp sym: e=" ++ (show e)
  case pos of
    Just (i,j) -> return $ M.Ld (i,j) : cs
    Nothing    -> return $ M.Ldg name : cs
comp env (CELL (SYM "quote") (CELL e NIL) ) cs  tail =
  liftIO $ (:) <$> S.itomCode (Ldc e) <*> return cs
comp env (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) cs tail = if tail
  then do
  tc <- comp env tb [M.Rtn] True
  ec <- comp env eb [M.Rtn] True
  comp env pred (M.Selr tc ec : cs) False
  else do
  tc <- comp env tb [M.Join] False
  ec <- comp env eb [M.Join] False
  comp env pred (M.Sel tc ec : cs) False
  
comp env (CELL (SYM "if") (CELL pred (CELL tb NIL ))) cs tail = if tail
  then do
  tc <- comp env tb [M.Rtn] True
  let ec = [M.Ldc M.NIL, M.Rtn]
  comp env pred (M.Sel tc ec : cs) False
  else do
  tc <- comp env tb [M.Join] False
  let ec = [M.Ldc M.NIL, M.Join]
  comp env pred (M.Sel tc ec : cs) False
  
comp (g,e) (CELL (SYM "lambda") (CELL args body)) cs tail = do
  debugPrint $ "comp lambda: args=" ++ (show args)
  debugPrint $ "comp lambda: body=" ++ (show body)
  args'  <- liftIO $ S.itom args >>= newIORef
  code <- compBody (g,(args':e)) body [M.Rtn]
  return $ M.Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs tail =
  comp env e (M.Def n : cs) False
comp env (CELL (SYM "define-macro") (CELL (SYM n) (CELL e NIL))) cs tail =
  comp env e (M.Defm n : cs) False
comp env@(g,e) (CELL (SYM "set!") (CELL (SYM n) (CELL v NIL))) cs tail = do
  pos <- liftIO $ findPos n e
  debugPrint $ "comp sym: sym=" ++ n
  debugPrint $ "comp sym: e=" ++ (show e)
  case pos of
    Just (i,j) -> comp env v (M.LSet (i,j) : cs) False
    Nothing    -> comp env v (M.GSet  n    : cs) False
  
comp env (CELL (SYM "quasiquote") (CELL e NIL) ) cs tail =
  translator 0 comp env e cs
comp env (CELL (SYM "unquote") (CELL e NIL) ) cs tail =
  throwE $ strMsg "unquote appeared outside quasiquote"
comp env (CELL (SYM "unquote-splicing") (CELL e NIL) ) cs tail =
  throwE $ strMsg "unquote-splicing appeared outside quasiquote"
comp env (CELL (SYM "call/cc") (CELL e NIL)) cs tail = do
  cs' <- comp env e (M.App:cs) False
  return $ [M.Ldct cs, M.Args 1] ++ cs'
comp env (CELL (SYM "apply") (CELL func args)) cs tail = do
  cs' <- comp env func (M.App:cs) False
  compArguments env args (M.ArgsAp (sExpLength args):cs')
  
comp env@(g,e) (CELL func@(SYM sym) args) cs tail = do
  x <- liftIO $ H.lookup g sym
  case x of
    Just (M.MACR' code e) -> do
      debugPrint $ "apply macro: sym=" ++ sym
      debugPrint $ "apply macro: code=" ++ (show code)
      debugPrint $ "apply macro: e=" ++ (show e)
      debugPrint $ "apply macro: args="  ++ (show args)
      args'  <- liftIO $ S.itom args >>= newIORef
      args'' <- S.exec g [] (args':e) code [M.Cont3 [] [] [M.Stop]]
      debugPrint $ "apply macro: args'="  ++ (show args')
      args''' <- liftIO $ S.mtoi args''
      comp env args''' cs False
    Just _ -> comp' env (CELL func args) cs tail
    Nothing -> comp' env (CELL func args) cs tail

comp env (CELL func args) cs tail = comp' env (CELL func args) cs tail

comp' env (CELL func args) cs tail = do
  cs' <- comp env func (if tail then M.TApp:cs else M.App:cs) False
  compArguments env args (M.Args (sExpLength args):cs')

compArguments :: M.CompilerProc'
compArguments env (CELL a as) cs = do
  cs' <- compArguments env as cs
  comp env a cs' False
compArguments _  NIL cs = return $ cs

compBody :: M.CompilerProc'
compBody env (CELL e NIL) cs = comp env e cs True
compBody env (CELL e es) cs = do
  cs' <- compBody env es cs
  comp env e (M.Pop : cs') False


findPos :: String -> M.Frame -> IO (Maybe (Int,Int))
findPos n xs = findFrame xs 0
  where
    findFrame :: M.Frame -> Int -> IO (Maybe (Int,Int))
    findFrame (f:fs) i = do
      x <- findCol f 0
      case x of
        Just j -> return $ Just (i,j)
        Nothing -> findFrame fs (i + 1)
    findFrame [] _  =   return Nothing
    findCol :: IORef M.SExpr -> Int -> IO (Maybe Int)
    findCol refx j = do
      x <- readIORef refx
      case x of
        (M.CELL refy refys) -> do
          y <- readIORef refy
          case y of
            (M.SYM m) -> if n == m
                         then return (Just j)
                         else findCol refys (j+1)
            _         -> findCol refys (j+1)
        (M.SYM m) -> if n == m
                     then return $ Just (-(j+1))
                     else return $ Nothing
        M.NIL     -> return Nothing

sExpLength :: SExpr -> Int
sExpLength (CELL h t) = 1 + sExpLength t
sExpLength NIL = 0

