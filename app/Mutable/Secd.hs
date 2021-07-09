

module Mutable.Secd where

import System.IO.Unsafe
import Data.IORef hiding (Eq)
import Mutable.SExpr
import qualified SExpr as I
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Error

trace :: Show a => String -> a -> a 
trace s x = unsafePerformIO ( do
                                putStrLn s
                                putStrLn $ "result " ++ show x
                                return x
                            )

getLVar :: Frame -> Int -> Int -> IO (IORef SExpr)
getLVar e i j = do
  let fr = e !! i
  if j < 0 
    then getItem' fr (-j)
    else do f' <- readIORef fr
            getItem  f' j  
  where getItem :: SExpr -> Int -> IO (IORef SExpr)
        getItem (CELL x xs) 0 = return x
        getItem (CELL x xs) n = do
          xs' <- readIORef xs
          getItem xs' (n-1)
        getItem  NIL n        = error "getItem: can't come here"
        getItem' :: (IORef SExpr) -> Int -> IO (IORef SExpr)
        getItem' v 1 = return v
        getItem' v n = do
          v' <- readIORef v
          case v' of
            (CELL x xs) -> getItem' xs (n-1)
            _           -> error "getItem': can't come here"

getGVar :: GEnv -> String -> Scm SExpr
getGVar g key = do
  x <- liftIO $ H.lookup g key 
  case x of
    Just v -> return v
    Nothing -> throwE $ strMsg $ "undefined value: " ++ key

exec :: GEnv -> Stack -> Frame -> [Code] -> Dump ->  Scm SExpr
exec g s e (Ld (i,j):c) d = do
  v <- liftIO $ getLVar e i j
  v' <- liftIO $ readIORef v
  exec g (v':s) e c d
    
exec g s e (Ldc v:c) d = exec g (v:s) e c d
exec g s e (Ldg sym:c) d = do
  v <- getGVar g sym
  exec g (v:s) e c d
exec g s e (Ldf code: c) d = exec g (CLOS' code e:s) e c d
exec g s e (Args n: c) d = do
  vs <- liftIO $ listToCell (reverse $ take n s)
  let s' = drop n s
  exec g (vs:s') e c d
                             
exec g s e (ArgsAp n: c) d = do
  vs <- liftIO $ listToCell (reverse $ take n s) >>= consOnLast
  let s' = drop n s
  exec g (vs:s') e c d
  where 
    consOnLast (CELL x xs) = do
      xs' <- readIORef xs
      case xs' of
        NIL -> return NIL
        _   -> CELL <$> (readIORef x >>= newIORef ) <*>
                        (consOnLast xs' >>= newIORef)
    consOnLast _           = error "bad args"
    copySExp   (CELL x xs) = CELL <$>
                             (readIORef x  >>= newIORef) <*>
                             (readIORef xs >>= copySExp >>= newIORef )
    copySExp           NIL = return NIL

exec g (CLOS' code e':vs:s) e (App:c) d  = do
  dumps <- liftIO $ dumpString s e c d
  debugPrint $ "App :" ++ dumps
  e'' <- liftIO $ (:) <$> newIORef vs <*> return e'
  exec g [] e'' code (Cont3 s e c:d)
exec g (CLOS' code e':vs:s) e (TApp:c) d = do
  dumps <- liftIO $ dumpString s e c d
  debugPrint $ "TApp :" ++ dumps
  e'' <- liftIO $ (:) <$> newIORef vs  <*> return e'
  exec g s e'' code d
exec g (v:s) e (Rtn:c) ((Cont3 s' e' c'):d) = exec g (v:s') e' c' d
exec g (PRIM' func:v:s) e (App:c) d  = do { v' <- func g v; exec g (v':s) e c d }
exec g (PRIM' func:v:s) e (TApp:c) d = do { v' <- func g v; exec g (v':s) e c d }
exec g (CONT (Cont3 s' e' c') d':((CELL v vs):s)) e (App:c) d  = do
  v' <- liftIO $ readIORef v
  exec g (v':s') e' c' d'
exec g (CONT (Cont3 s' e' c') d':((CELL v vs):s)) e (TApp:c) d = do
  v' <- liftIO $ readIORef v
  exec g (v':s') e' c' d'

exec g (BOOL b:s) e (Sel ct cf:c) d  = if b then exec g s e ct (Cont1 c:d)
                                       else exec g s e cf (Cont1 c:d)
exec g (BOOL b:s) e (Selr ct cf:c) d = if b then exec g s e ct d
                                       else exec g s e cf d
exec g s e (Join:[]) (Cont1 c:d) = exec g s e c d
exec g (v:s) e (Pop:c) d = exec g s e c d
exec g (v:s) e (Def sym:c) d = do liftIO $ H.insert g sym v  
                                  exec g (SYM sym:s) e c d
exec g (CLOS' code e':s) e (Defm sym:c) d = do liftIO $ H.insert g sym (MACR' code e')
                                               exec g (SYM sym:s) e c d
exec g s e (Ldct cs:c) d = exec g (CONT (Cont3 s e cs) d:s) e c d
                           
exec g (v:s) e (Stop:c) d =  return v
exec g s e (Dump:c) d =  liftIO $ (dumpString s e c d >>= (return . STR) )
exec g s e c d = do
  dumps <- liftIO $ dumpString s e c d
  throwE $ strMsg $ "exec failure: " ++ dumps
                 
dumpString :: [SExpr] -> [IORef SExpr] -> [Code] -> [Cont] -> IO String
dumpString s e c d = do 
  e' <- mapM readIORef e 
  return $ "s=" ++ (show s) ++ "," ++ 
           "e=" ++ (show e') ++ "," ++ 
           "c=" ++ (show c) ++ "," ++                         
           "d=" ++ (show d)

listToCell :: [SExpr] -> IO SExpr
listToCell (x:xs) = CELL <$> newIORef x <*> (listToCell xs >>= newIORef )
listToCell []     = return NIL

cellToList :: SExpr -> IO [SExpr]
cellToList (CELL x xs) = do
  xs' <- readIORef xs
  case xs' of
    NIL -> (:) <$> readIORef x <*> return []
    _ -> (:) <$> readIORef x <*> cellToList xs'

mtoi :: SExpr -> IO I.SExpr
mtoi (INT  x) = return (I.INT  x)
mtoi (REAL x) = return (I.REAL x)
mtoi (SYM x)  = return (I.SYM  x)
mtoi (STR x)  = return (I.STR  x)
mtoi (BOOL x) = return (I.BOOL x)
mtoi  NIL     = return  I.NIL
mtoi (CELL x xs) = I.CELL <$> (readIORef x >>= mtoi) <*> (readIORef xs >>= mtoi)

itom :: I.SExpr -> IO SExpr
itom (I.INT  x) = return (INT  x)
itom (I.REAL x) = return (REAL x)
itom (I.SYM x)  = return (SYM  x)
itom (I.STR x)  = return (STR  x)
itom (I.BOOL x) = return (BOOL x)
itom  I.NIL     = return  NIL
itom (I.CELL x xs) = CELL <$> (itom x >>= newIORef) <*> (itom xs >>= newIORef )

-- prim :: I.SecdFunc ->  SecdFunc
-- prim f = \g e -> do
--   e' <- liftIO $ mtoi e
--   v' <- f g e'
--   liftIO $ itom v'
  
       




