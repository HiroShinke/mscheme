
module Mutable.Translator where

import SExpr
import qualified Mutable.SExpr as M
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Error
import qualified Mutable.SecdFuncs as F

-- comp is Compiler.comp passed as argument

translator :: Int -> M.CompilerProc -> M.TranslatorProc
translator n comp env ls@(CELL (CELL _ _) _) cs = translatorList n comp env ls cs
translator n comp env ls@(CELL _ _) cs = translatorAtom n comp env ls cs
translator n comp env e cs = do
  debugPrint $ "translator.n: env=" ++ (show env)
  debugPrint $ "translator.n: e=" ++ (show e)
  e' <- liftIO $ M.itom e
  return $ M.Ldc e' : cs


translatorList :: Int -> M.CompilerProc -> M.TranslatorProc
translatorList n comp env ls@(CELL (CELL (SYM "unquote") _) _) cs =
  translatorUnquote n comp env ls cs
translatorList n comp env ls@(CELL (CELL (SYM "unquote-splicing") _) _) cs =
  translatorUnquoteSplicing n comp env ls cs
translatorList n comp env ls@(CELL (CELL (SYM "quasiquote") _) _) cs =
  translatorQuasiquote n comp env ls cs
translatorList n comp env (CELL x xs) cs = do
  debugPrint $ "!!!!!: x=" ++ (show x)
  debugPrint $ "!!!!!: xs=" ++ (show xs)  
  c' <- translator n comp env x []
  cs' <- translator n comp env xs []
  return $ (consCode c' cs') ++ cs
translatorList _ _ _ _ _ = throwE $ strMsg "shouldn't come here"

translatorSub :: SExpr -> Int -> Int -> M.CompilerProc -> M.TranslatorProc
translatorSub sym n succ comp env e cs = do
  cs' <- translator (n + succ) comp env e []
  sym' <- liftIO $ M.itom sym
  return $ consCode [M.Ldc sym'] (consCode cs' [M.Ldc M.NIL]) ++ cs

translatorUnquote :: Int -> M.CompilerProc -> M.TranslatorProc
translatorUnquote 0 comp env (CELL (CELL (SYM "unquote") (CELL x NIL)) xs) cs = do
  x' <- comp env x [] False
  xs' <- translator 0 comp env xs []
  return $ consCode x' xs' ++ cs

translatorUnquote n comp env (CELL (CELL (SYM "unquote") (CELL e NIL)) xs) cs = do
  c' <- translatorSub unquote n (-1) comp env e []
  cs' <- translator n comp env xs []
  return $ consCode c' cs' ++ cs

translatorUnquoteSplicing :: Int -> M.CompilerProc -> M.TranslatorProc
translatorUnquoteSplicing 0 comp env (CELL (CELL (SYM "unquote-splicing") (CELL x NIL)) xs) cs = do
  x' <- comp env x [] False
  xs' <- translator 0 comp env xs []
  return $ appendCode x' xs' ++ cs
    
translatorUnquoteSplicing n comp env (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) xs) cs = do
  e' <- translatorSub unquoteSplicing n (-1) comp env e []
  xs' <- translator n comp env xs []
  return $ consCode e' xs' ++ cs

translatorQuasiquote :: Int -> M.CompilerProc -> M.TranslatorProc
translatorQuasiquote n comp env (CELL (CELL (SYM "quasiquote") (CELL e NIL)) xs) cs = do
  e' <- translatorSub quasiquote n 1 comp env e []
  xs' <- translator n comp env xs []
  return $ consCode e' xs' ++ cs

translatorAtom :: Int -> M.CompilerProc -> M.TranslatorProc
translatorAtom 0 comp env (CELL (SYM "unquote") (CELL e NIL)) cs = do
  debugPrint $ "translatorAtom e=" ++ (show e)
  debugPrint $ "translatorAtom comp env=" ++ (show env)
  comp env e cs False
  
translatorAtom 1 comp env (CELL (SYM "unquote")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e [] False
  return $ consCode [M.Ldc (M.SYM "unquote")] cs' ++ cs

translatorAtom n comp env (CELL (SYM "unquote") (CELL e NIL)) cs = 
  translatorSub unquote n (-1) comp env e cs
translatorAtom 0 comp env (CELL (SYM "unquote-splicing") _) cs =
  throwE $ strMsg "invalid unquote-splicing form"
translatorAtom 1 comp env (CELL (SYM "unquote-splicing")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e [] False
  return $ consCode [M.Ldc (M.SYM "unquote-splicing")] cs' ++ cs

translatorAtom  n comp env (CELL (SYM "unquote-splicing") (CELL e NIL)) cs =
  translatorSub unquoteSplicing n (-1) comp env e cs
translatorAtom n comp env (CELL (SYM "quasiquote") (CELL e NIL)) cs =
  translatorSub quasiquote n 1 comp env e cs
translatorAtom n comp env (CELL e xs) cs = do
  debugPrint $ "translatorAtom.n: comp env=" ++ (show env)
  debugPrint $ "translatorAtom.n: e=" ++ (show e)
  debugPrint $ "translatorAtom.n: xs=" ++ (show xs)
  xs' <- translator n comp env xs []
  e' <- liftIO $ M.itom e
  debugPrint $ "translatorAtom.n: xs'=" ++ (show xs')  
  return $ consCode [M.Ldc e'] xs' ++ cs

---- helper function

consCode :: [M.Code] -> [M.Code] -> [M.Code]
consCode cs1 cs2 = cs1 ++ cs2 ++ [M.Args 2, M.Ldc (M.PRIM' F.cons), M.App]

appendCode :: [M.Code] -> [M.Code] -> [M.Code]
appendCode cs1 cs2 = cs1 ++ cs2 ++ [M.Args 2, M.Ldc (M.PRIM' F.append'), M.App]
