

module Evaluator where

import Data.IORef

import Control.Monad.Trans.Except
import System.IO
import qualified Secd as S
import Compiler
import Primitives
import Error

import qualified Data.HashTable.IO as H

pushLEnv :: String -> SExpr -> LEnv -> IO LEnv
pushLEnv s v env = do
  a <- v `seq` newIORef v
  return ((s, a):env)

lookupLEnv :: String -> LEnv -> IO (Maybe SExpr)
lookupLEnv s env = 
  case lookup s env of
    Nothing -> return Nothing
    Just v  -> do a <- readIORef v
                  return (Just a)

updateLEnv :: String -> SExpr -> LEnv -> IO (LEnv)
updateLEnv s v env =
  case lookup s env of
    Nothing -> pushLEnv s v env
    Just a  -> do writeIORef a v
                  return env


lift :: (Monad m) => m a -> ExceptT e m a
lift = ExceptT . liftM Right 
                     
-- | Promote a function to a monad.
liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f ma = do a <- ma
                return (f a)

liftIO = lift                

--
-- S 式の評価
--
eval :: ScmFunc
eval env NIL        = return NIL
eval env v@(INT _)  = return v
eval env v@(REAL _) = return v
eval env v@(STR _)  = return v
eval env (SYM name) = do
  a <- liftIO $ lookupLEnv name $ snd env
  case a of
    Nothing -> do b <- liftIO $ H.lookup (fst env) name
                  case b of
                    Nothing -> throwE $ strMsg $ "unbound variable: " ++ name
                    Just v  -> return v
    Just v -> return v
eval env (CELL func args) = do
  v <- eval env func
  case v of
    SYNT f -> f env args
    MACR f -> do expr <- apply env f args
                 eval env expr
    _      -> do vs <- evalArguments env args
                 apply env v vs

-- 引数の評価
evalArguments :: ScmFunc
evalArguments env NIL = return NIL
evalArguments env (CELL expr rest) = do
  v  <- eval env expr
  vs <- evalArguments env rest
  return (CELL v vs)
evalArguments _ _ = throwE $ strMsg "invalid function form"

-- 変数束縛
makeBindings :: LEnv -> SExpr -> SExpr -> Scm LEnv
makeBindings lenv NIL        _    = return lenv
makeBindings lenv (SYM name) rest = liftIO $ pushLEnv name rest lenv
makeBindings lenv (CELL (SYM name) parms) (CELL v args) = do
  lenv' <- makeBindings lenv parms args
  liftIO (pushLEnv name v lenv')
makeBindings lenv _ NIL = throwE $ strMsg errNEA
makeBindings lenv _ _   = throwE $ strMsg "invalid arguments form"

-- 関数適用
apply :: Env -> SExpr -> SExpr -> Scm SExpr
apply env func actuals =
  case func of
    PRIM f -> f env actuals
    CLOS (CELL parms body) lenv0 -> do
      lenv1 <- makeBindings lenv0 parms actuals
      evalBody (fst env, lenv1) body
    _ -> throwE $ strMsg $ "Not Function: " ++ show func

-- 本体の評価
evalBody :: ScmFunc
evalBody env (CELL expr NIL) = eval env expr
evalBody env (CELL expr rest) = do
  eval env expr
  evalBody env rest
evalBody _ _ = throwE $ strMsg "invalid body form"

--
-- シンタックス形式
--

-- quote
evalQuote :: ScmFunc
evalQuote env (CELL expr _) = return expr
evalQuote _ _ = throwE $ strMsg "invalid quote form"


-- define
evalDef :: ScmFunc
evalDef env (CELL sym@(SYM name) (CELL expr NIL)) = do
  v <- eval env expr
  liftIO $ H.insert (fst env) name v
  return sym
evalDef _ _ = throwE $ strMsg "invalid define form"

-- define-macro
evalDefM :: ScmFunc
evalDefM env (CELL sym@(SYM name) (CELL expr NIL)) = do
  v <- eval env expr
  liftIO $ H.insert (fst env) name (MACR v)
  return sym
evalDefM _ _ = throwE $ strMsg "invalid define form"


-- if
evalIf :: ScmFunc
evalIf env (CELL pred (CELL thenForm rest)) = do
  v <- eval env pred
  if v /= false
  then eval env thenForm
  else case rest of
         CELL elseForm _ -> eval env elseForm
         _               -> return false
evalIf _ _ = throwE $ strMsg $ "if : " ++ errNEA

-- lambda
evalLambda :: ScmFunc
evalLambda env expr = return (CLOS expr (snd env))

-- set!
evalSet :: ScmFunc
evalSet env (CELL (SYM name) (CELL expr _)) = do
  v <- eval env expr
  a <- liftIO $ lookupLEnv name (snd env)
  case a of
    Nothing -> do b <- liftIO $ H.lookup (fst env) name
                  case b of
                    Nothing -> throwE $ strMsg $ "unbound variable: " ++ name
                    Just _ -> do liftIO $ H.insert (fst env) name v
                                 return v
    Just _  -> do liftIO $ updateLEnv name v (snd env)
                  return v
evalSet _ _ = throwE (strMsg "invalid set! form")


--- unquotes

-- helpers
listOf2 :: SExpr -> SExpr -> SExpr
listOf2 x y = (CELL x (CELL y NIL))

listOf3 :: SExpr -> SExpr -> SExpr -> SExpr
listOf3 x y z = (CELL x (CELL y (CELL z NIL)))


unquote' :: ScmFunc
unquote' _ _ = throwE $ strMsg "unquote appeared outside quasiquote"

unquoteSplicing' :: ScmFunc
unquoteSplicing' _ _ = throwE $ strMsg "unquote-splicing appeared outside quasiquote"

quasiquote' :: ScmFunc
quasiquote' env (CELL e nil) = translator 0 env e

translator :: Int -> ScmFunc
translator n env ls@(CELL (CELL _ _) _) = translatorList n env ls
translator n env ls@(CELL _ _) = translatorAtom n env ls
translator n env x = return x
  
translatorList :: Int -> ScmFunc
translatorList n env ls@(CELL (CELL (SYM "unquote") _) _) = translatorUnquote n env ls
translatorList n env ls@(CELL (CELL (SYM "unquote-splicing") _) _) = translatorUnquoteSplicing n env ls
translatorList n env ls@(CELL (CELL (SYM "quasiquote") _) _) = translatorQuasiquote n env ls
translatorList n env (CELL x xs) = do
  x' <- translator n env x
  xs' <- translator n env xs
  return (CELL x' xs')
translatorList _ _ _ = throwE $ strMsg "shouldn't come here"

translatorSub :: Env -> SExpr -> SExpr -> Int -> Int -> Scm SExpr
translatorSub env sym e n succ = do
  e' <- translator (n + succ) env e
  return (CELL sym (CELL e' NIL))

translatorUnquote :: Int -> ScmFunc
translatorUnquote 0 env (CELL (CELL (SYM "unquote") (CELL e NIL)) xs) = do
  e' <- eval env e
  xs' <- translator 0 env xs
  return (CELL e' xs')

translatorUnquote n env (CELL (CELL (SYM "unquote") (CELL e NIL)) xs) = do
  e' <- translatorSub env unquote e n (-1)
  xs' <- translator n env xs
  return (CELL e' xs')

translatorUnquoteSplicing :: Int -> ScmFunc
translatorUnquoteSplicing 0 env (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) xs) = do
  e' <- eval env e
  xs' <- translator 0 env xs
  append' env (CELL e' (CELL xs' NIL))
    
translatorUnquoteSplicing n env (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) xs) = do
  e' <- translatorSub env unquoteSplicing e n (-1)
  xs' <- translator n env xs
  return (CELL e' xs')
 
translatorQuasiquote :: Int -> ScmFunc
translatorQuasiquote n env (CELL (CELL (SYM "quasiquote") (CELL e NIL)) xs) = do
  e' <- translatorSub env quasiquote e n 1
  xs' <- translator n env xs
  return (CELL e' xs')

translatorAtom :: Int -> ScmFunc
translatorAtom 0 env (CELL (SYM "unquote") (CELL e NIL)) = eval env e
  
translatorAtom 1 env (CELL (SYM "unquote") (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) = do
   e' <- eval env e
   return (CELL unquote e')
translatorAtom n env (CELL (SYM "unquote") (CELL e NIL)) = 
  translatorSub env unquote e n (-1)
translatorAtom 0 env (CELL (SYM "unquote-splicing") _) =
  throwE $ strMsg "invalid unquote-splicing form"
translatorAtom 1 env (CELL (SYM "unquote-splicing") (CELL (CELL (SYM "unquote-splicing") (CELL e NIL)) NIL)) = do
  e' <- eval env e
  return (CELL unquoteSplicing e' )
translatorAtom  n env (CELL (SYM "unquote-splicing") (CELL e NIL)) =
  translatorSub env unquoteSplicing e n (-1)
translatorAtom n env (CELL (SYM "quasiquote") (CELL e NIL)) =
  translatorSub env quasiquote e n 1
translatorAtom n env (CELL e xs) = do
  xs' <- translator n env xs
  return (CELL e xs')

