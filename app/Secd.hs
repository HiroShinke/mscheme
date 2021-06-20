

module Secd where

import System.IO.Unsafe

trace :: Show a => String -> a -> a 
trace s x = unsafePerformIO ( do
                                putStrLn s
                                putStrLn $ "result " ++ show x
                                return x
                            )
            
data Value = Num Int
           | VBool Bool
           | Closure [Code] Env
           | Prim (Value->Value)
           | Cell Value Value
           | Str String
           | Sym String
           | Nil

instance Show Value where
  show (Prim _) = "Primitive"
  show (Num n) = "Num " ++ (show n)
  show (VBool n) = show n
  show (Closure cs e) =
    "Colsure " ++ (show cs) ++ " " ++ (show e)
  show (Cell x xs) = "List " ++ (show x) ++ " " ++ (show xs)
  show (Str s) = "Str " ++ (show s)
  show (Sym s) = "Sym " ++ (show s)
  show Nil = "Nil"

instance Eq Value where
  Num x == Num y = x == y
  Str x == Str y = x == y  
  Sym x == Sym y = x == y
  VBool x == VBool y = x == y
  Nil   == Nil   = True
  Cell x xs == Cell y ys = x == y && xs == ys
  _ == _             = False
  
type Env   = [[Value]]
type Stack = [Value]
type Dump  = [Cont]
type GEnv  = [(String,Value)]

data Cont = Cont3 Stack Env [Code]
          | Cont1 [Code]
  deriving Show

data Code = Ld (Int,Int)
          | Ldc Value
          | Ldg String
          | Ldf [Code]
          | Args Int
          | App
          | Rtn
          | Sel [Code] [Code]
          | Join
          | Pop
          | Def String
          | Stop
          | Dump
  deriving (Show,Eq)


globalEnv :: [(String,Value)]
globalEnv = []

getLVar :: Env -> Int -> Int -> Value
getLVar e i j = e !! i !! j
getGVar :: [(String,Value)] -> String -> Value
getGVar g key = case lookup key g of
                  Just v -> v
                  Nothing -> Nil

exec :: GEnv -> Stack -> Env -> [Code] -> Dump ->  Value
exec g s e (Ld (i,j):c) d = exec g (v:s) e c d where v = getLVar e i j
exec g s e (Ldc v:c) d = exec g (v:s) e c d
exec g s e (Ldg sym:c) d = exec g (v:s) e c d where v = getGVar g sym
exec g s e (Ldf code: c) d = exec g (Closure code e:s) e c d
exec g s e (Args n: c) d = exec g (vs:s') e c d where vs = listToCell(take n s)
                                                      s' = drop n s
exec g (Closure code e':vs:s) e (App:c) d = exec g [] (cellToList vs:e') code (Cont3 s e c:d)
exec g (v:s) e (Rtn:c) ((Cont3 s' e' c'):d) = exec g (v:s') e' c' d
exec g (Prim func:v:s) e (App:c) d = exec g (func v:s) e c d
exec g (VBool b:s) e (Sel ct cf:c) d = if b then exec g s e ct (Cont1 c:d)
                                       else exec g s e cf (Cont1 c:d)
exec g s e (Join:[]) (Cont1 c:d) = exec g s e c d
exec g (v:s) e (Pop:c) d = exec g s e c d
exec g (v:s) e (Def sym:c) d = exec g' (Sym sym:s) e c d where g' = (sym,v) : g
exec g (v:s) e (Stop:c) d =  v
exec g s e (Dump:c) d =  Str $ (dumpString s e c d)
exec g s e c d = error $ "exec failure: " ++ (dumpString s e c d)
                 
dumpString s e c d =  "s=" ++ (show s) ++ "," ++ 
                      "e=" ++ (show e) ++ "," ++ 
                      "c=" ++ (show c) ++ "," ++                         
                      "d=" ++ (show d)

listToCell :: [Value] -> Value
listToCell (x:xs) = Cell x (listToCell xs)
listToCell []     = Nil

cellToList :: Value -> [Value]
cellToList (Cell x xs) = x : (cellToList xs)
cellToList  Nil        = []
