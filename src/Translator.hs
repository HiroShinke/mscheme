
module Translator where

import SExpr
import Control.Monad.Trans.Except
import Error
import qualified SecdFuncs as F

-- comp is Compiler.comp passed as argument

translator :: Int -> CompilerProc -> TranslatorProc
translator n comp env ls@(CELL (CELL _ _) _) cs = translatorList n comp env ls cs
translator n comp env ls@(CELL _ _) cs = translatorAtom n comp env ls cs
translator n comp env e cs = do
  debugPrint $ "translator.n: env=" ++ (show env)
  debugPrint $ "translator.n: e=" ++ (show e)
  return $ Ldc e : cs


translatorList :: Int -> CompilerProc -> TranslatorProc
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

translatorSub :: SExpr -> Int -> Int -> CompilerProc -> TranslatorProc
translatorSub sym n succ comp env e cs = do
  cs' <- translator (n + succ) comp env e []
  return $ consCode [Ldc sym] (consCode cs' [Ldc NIL]) ++ cs

translatorUnquote :: Int -> CompilerProc -> TranslatorProc
translatorUnquote 0 comp env (CELL (CELL (SYM "unquote") (CELL x NIL)) xs) cs = do
  x' <- comp env x [] False
  xs' <- translator 0 comp env xs []
  return $ consCode x' xs' ++ cs

translatorUnquote n comp env (CELL (CELL (SYM "unquote") (CELL e NIL)) xs) cs = do
  c' <- translatorSub unquote n (-1) comp env e []
  cs' <- translator n comp env xs []
  return $ consCode c' cs' ++ cs

translatorUnquoteSplicing :: Int -> CompilerProc -> TranslatorProc
translatorUnquoteSplicing 0 comp env (CELL (CELL (SYM "unquote-splicing") (CELL x NIL)) xs) cs = do
  x' <- comp env x [] False
  xs' <- translator 0 comp env xs []
  return $ appendCode x' xs' ++ cs
    
translatorUnquoteSplicing n comp env (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) xs) cs = do
  e' <- translatorSub unquoteSplicing n (-1) comp env e []
  xs' <- translator n comp env xs []
  return $ consCode e' xs' ++ cs

translatorQuasiquote :: Int -> CompilerProc -> TranslatorProc
translatorQuasiquote n comp env (CELL (CELL (SYM "quasiquote") (CELL e NIL)) xs) cs = do
  e' <- translatorSub quasiquote n 1 comp env e []
  xs' <- translator n comp env xs []
  return $ consCode e' xs' ++ cs

translatorAtom :: Int -> CompilerProc -> TranslatorProc
translatorAtom 0 comp env (CELL (SYM "unquote") (CELL e NIL)) cs = do
  debugPrint $ "translatorAtom e=" ++ (show e)
  debugPrint $ "translatorAtom comp env=" ++ (show env)
  comp env e cs False
  
translatorAtom 1 comp env (CELL (SYM "unquote")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e [] False
  return $ consCode [Ldc (SYM "unquote")] cs' ++ cs

translatorAtom n comp env (CELL (SYM "unquote") (CELL e NIL)) cs = 
  translatorSub unquote n (-1) comp env e cs
translatorAtom 0 comp env (CELL (SYM "unquote-splicing") _) cs =
  throwE $ strMsg "invalid unquote-splicing form"
translatorAtom 1 comp env (CELL (SYM "unquote-splicing")
                      (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) cs = do
  cs' <- comp env e [] False
  return $ consCode [Ldc (SYM "unquote-splicing")] cs' ++ cs

translatorAtom  n comp env (CELL (SYM "unquote-splicing") (CELL e NIL)) cs =
  translatorSub unquoteSplicing n (-1) comp env e cs
translatorAtom n comp env (CELL (SYM "quasiquote") (CELL e NIL)) cs =
  translatorSub quasiquote n 1 comp env e cs
translatorAtom n comp env (CELL e xs) cs = do
  debugPrint $ "translatorAtom.n: comp env=" ++ (show env)
  debugPrint $ "translatorAtom.n: e=" ++ (show e)
  debugPrint $ "translatorAtom.n: xs=" ++ (show xs)
  xs' <- translator n comp env xs []
  debugPrint $ "translatorAtom.n: xs'=" ++ (show xs')  
  return $ consCode [Ldc e] xs' ++ cs

---- helper function

consCode :: [Code] -> [Code] -> [Code]
consCode cs1 cs2 = cs1 ++ cs2 ++ [Args 2, Ldc (PRIM' F.cons), App]

appendCode :: [Code] -> [Code] -> [Code]
appendCode cs1 cs2 = cs1 ++ cs2 ++ [Args 2, Ldc (PRIM' F.append'), App]
