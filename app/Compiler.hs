

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

debugPrintOn = False

debugPrint :: String -> Scm ()
debugPrint msg = if debugPrintOn
                 then liftIO $ putStrLn $ msg
                 else return ()

--- compile

compile :: GEnv -> SExpr -> Scm [Code]
compile g expr = comp (g,[]) expr [Stop]

comp :: Env' -> SExpr -> [Code] -> Scm [Code]
comp env v@NIL        cs = return $ Ldc v : cs
comp env v@(INT n)    cs = return $ Ldc v : cs
comp env v@(STR n)    cs = return $ Ldc v : cs
comp env v@(BOOL n)   cs = return $ Ldc v : cs
comp (g,e) v@(SYM name) cs = let pos = findPos name e
                             in do
  debugPrint $ "comp sym: sym=" ++ name
  debugPrint $ "comp sym: e=" ++ (show e)
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
  debugPrint $ "comp lambda: args=" ++ (show args)
  debugPrint $ "comp lambda: body=" ++ (show body)  
  code <- compBody (g,(args:e)) body [Rtn]
  return $ Ldf code : cs
comp env (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Def n : cs)
comp env (CELL (SYM "define-macro") (CELL (SYM n) (CELL e NIL))) cs =
  comp env e (Defm n : cs)
comp env (CELL (SYM "quasiquote") (CELL e NIL) ) cs =
  translator 0 env e cs
comp env (CELL (SYM "unquote") (CELL e NIL) ) cs =
  throwE $ strMsg "unquote appeared outside quasiquote"
comp env (CELL (SYM "unquote-splicing") (CELL e NIL) ) cs =
  throwE $ strMsg "unquote-splicing appeared outside quasiquote"
comp env (CELL (SYM "call/cc") (CELL e NIL)) cs = do
  cs' <- comp env e (App:cs)
  return $ [Ldct cs, Args 1] ++ cs'
comp env (CELL (SYM "apply") (CELL func args)) cs = do
  cs' <- comp env func (App:cs) 
  compArguments env args (ArgsAp (sExpLength args):cs')
  
comp env@(g,e) (CELL func@(SYM sym) args) cs = do
  x <- liftIO $ H.lookup g sym
  case x of
    Just (MACR' code e) -> do
      debugPrint $ "apply macro: sym=" ++ sym
      debugPrint $ "apply macro: code=" ++ (show code)
      debugPrint $ "apply macro: e=" ++ (show e)
      debugPrint $ "apply macro: args="  ++ (show args)
      args' <- S.exec g [] (args:e) code [Cont3 [] [] [Stop]]
      debugPrint $ "apply macro: args'="  ++ (show args')
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

translator :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translator n env ls@(CELL (CELL _ _) _) cs = translatorList n env ls cs
translator n env ls@(CELL _ _) cs = translatorAtom n env ls cs
translator n env e cs = do
  debugPrint $ "translator.n: env=" ++ (show env)
  debugPrint $ "translator.n: e=" ++ (show e)
  return $ Ldc e : cs


translatorList :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translatorList n env ls@(CELL (CELL (SYM "unquote") _) _) cs =
  translatorUnquote n env ls cs
translatorList n env ls@(CELL (CELL (SYM "unquote-splicing") _) _) cs =
  translatorUnquoteSplicing n env ls cs
translatorList n env ls@(CELL (CELL (SYM "quasiquote") _) _) cs =
  translatorQuasiquote n env ls cs
translatorList n env (CELL x xs) cs = do
  debugPrint $ "!!!!!: x=" ++ (show x)
  debugPrint $ "!!!!!: xs=" ++ (show xs)  
  c' <- translator n env x []
  cs' <- translator n env xs []
  return $ (consCode c' cs') ++ cs
translatorList _ _ _ _ = throwE $ strMsg "shouldn't come here"

translatorSub :: Env' -> SExpr -> SExpr -> Int -> Int -> [Code] -> Scm [Code]
translatorSub env sym e n succ cs = do
  cs' <- translator (n + succ) env e []
  return $ consCode [Ldc sym] (consCode cs' [Ldc NIL]) ++ cs

translatorUnquote :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translatorUnquote 0 env (CELL (CELL (SYM "unquote") (CELL x NIL)) xs) cs = do
  x' <- comp env x []
  xs' <- translator 0 env xs []
  return $ consCode x' xs' ++ cs

translatorUnquote n env (CELL (CELL (SYM "unquote") (CELL e NIL)) xs) cs = do
  c' <- translatorSub env unquote e n (-1) []
  cs' <- translator n env xs []
  return $ consCode c' cs' ++ cs

translatorUnquoteSplicing :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translatorUnquoteSplicing 0 env (CELL (CELL (SYM "unquote-splicing") (CELL x NIL)) xs) cs = do
  x' <- comp env x []
  xs' <- translator 0 env xs []
  return $ appendCode x' xs' ++ cs
    
translatorUnquoteSplicing n env (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) xs) cs = do
  e' <- translatorSub env unquoteSplicing e n (-1) []
  xs' <- translator n env xs []
  return $ consCode e' xs' ++ cs

translatorQuasiquote :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translatorQuasiquote n env (CELL (CELL (SYM "quasiquote") (CELL e NIL)) xs) cs = do
  e' <- translatorSub env quasiquote e n 1 []
  xs' <- translator n env xs []
  return $ consCode e' xs' ++ cs

translatorAtom :: Int -> Env' -> SExpr -> [Code] -> Scm [Code]
translatorAtom 0 env (CELL (SYM "unquote") (CELL e NIL)) cs = do
  debugPrint $ "translatorAtom e=" ++ (show e)
  debugPrint $ "translatorAtom env=" ++ (show env)
  comp env e cs
  
translatorAtom 1 env (CELL (SYM "unquote")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e []
  return $ consCode [Ldc (SYM "unquote")] cs' ++ cs

translatorAtom n env (CELL (SYM "unquote") (CELL e NIL)) cs = 
  translatorSub env unquote e n (-1) cs
translatorAtom 0 env (CELL (SYM "unquote-splicing") _) cs =
  throwE $ strMsg "invalid unquote-splicing form"
translatorAtom 1 env (CELL (SYM "unquote-splicing")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e []
  return $ consCode [Ldc (SYM "unquote-splicing")] cs' ++ cs

translatorAtom  n env (CELL (SYM "unquote-splicing") (CELL e NIL)) cs =
  translatorSub env unquoteSplicing e n (-1) cs
translatorAtom n env (CELL (SYM "quasiquote") (CELL e NIL)) cs =
  translatorSub env quasiquote e n 1 cs
translatorAtom n env (CELL e xs) cs = do
  debugPrint $ "translatorAtom.n: env=" ++ (show env)
  debugPrint $ "translatorAtom.n: e=" ++ (show e)
  debugPrint $ "translatorAtom.n: xs=" ++ (show xs)
  xs' <- translator n env xs []
  debugPrint $ "translatorAtom.n: xs'=" ++ (show xs')  
  return $ consCode [Ldc e] xs' ++ cs

---- helper function

consCode :: [Code] -> [Code] -> [Code]
consCode cs1 cs2 = cs1 ++ cs2 ++ [Args 2, Ldc (PRIM' F.cons), App]

appendCode :: [Code] -> [Code] -> [Code]
appendCode cs1 cs2 = cs1 ++ cs2 ++ [Args 2, Ldc (PRIM' F.append'), App]
