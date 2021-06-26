

module Secd where

import System.IO.Unsafe
import SExpr
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
            
getLVar :: Frame -> Int -> Int -> SExpr
getLVar e i j = let frm = e !! i
                in  getItem frm j
  where getItem (CELL x xs) 0 = x
        getItem (CELL x xs) n = getItem xs (n-1)
        getItem  NIL n        = error "can't come here"

getGVar :: GEnv -> String -> Scm SExpr
getGVar g key = do
  x <- liftIO $ H.lookup g key 
  case x of
    Just v -> return v
    Nothing -> throwE $ strMsg $ "undefined value: " ++ key

exec :: GEnv -> Stack -> Frame -> [Code] -> Dump ->  Scm SExpr
exec g s e (Ld (i,j):c) d = exec g (v:s) e c d where v = getLVar e i j
exec g s e (Ldc v:c) d = exec g (v:s) e c d
exec g s e (Ldg sym:c) d = do
  v <- getGVar g sym
  exec g (v:s) e c d
exec g s e (Ldf code: c) d = exec g (CLOS' code e:s) e c d
exec g s e (Args n: c) d = exec g (vs:s') e c d where vs = listToCell(reverse $ take n s)
                                                      s' = drop n s
exec g (CLOS' code e':vs:s) e (App:c) d = exec g [] (vs:e') code (Cont3 s e c:d)
exec g (v:s) e (Rtn:c) ((Cont3 s' e' c'):d) = exec g (v:s') e' c' d
exec g (PRIM' func:v:s) e (App:c) d = do
  v' <- func g v
  exec g (v':s) e c d
exec g (BOOL b:s) e (Sel ct cf:c) d = if b then exec g s e ct (Cont1 c:d)
                                       else exec g s e cf (Cont1 c:d)
exec g s e (Join:[]) (Cont1 c:d) = exec g s e c d
exec g (v:s) e (Pop:c) d = exec g s e c d
exec g (v:s) e (Def sym:c) d = do liftIO $ H.insert g sym v  
                                  exec g (SYM sym:s) e c d
exec g (CLOS' code e':s) e (Defm sym:c) d = do liftIO $ H.insert g sym (MACR' code e')
                                               exec g (SYM sym:s) e c d
exec g (v:s) e (Stop:c) d =  return v
exec g s e (Dump:c) d =  return $ STR $ (dumpString s e c d)
exec g s e c d = throwE $ strMsg $ "exec failure: " ++ (dumpString s e c d)
                 
dumpString s e c d =  "s=" ++ (show s) ++ "," ++ 
                      "e=" ++ (show e) ++ "," ++ 
                      "c=" ++ (show c) ++ "," ++                         
                      "d=" ++ (show d)

listToCell :: [SExpr] -> SExpr
listToCell (x:xs) = CELL x (listToCell xs)
listToCell []     = NIL

cellToList :: SExpr -> [SExpr]
cellToList (CELL x xs) = x : (cellToList xs)
cellToList  NIL       = []
