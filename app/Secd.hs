

module Secd where

data Value = Num Int
           | VBool Bool
           | Closure [Code] Env
           | Prim ([Value]->Value)
           | List [Value]
           | Str String
           | Sym String
           | Nil

instance Show Value where
  show (Prim _) = "Primitive"
  show (Num n) = show n
  show (VBool n) = show n
  show (Closure _ _) = "Colsure"
  show (List xs) = show xs
  show (Str s) = show s
  show (Sym s) = show s
  show Nil = "Nil"

type Env   = [[Value]]
type Stack = [Value]
type Dump  = [Cont]
type GEnv  = [(String,Value)]

data Cont = Cont3 Stack Env [Code]
          | Cont1 [Code]

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
  deriving Show


globalEnv :: [(String,Value)]
globalEnv = []

getLVar :: Env -> Int -> Int -> Value
getLVar = undefined
getGVar :: [(String,Value)] -> String -> Value
getGVar g key = case lookup key g of
                  Just v -> v
                  Nothing -> Nil

exec :: GEnv -> Stack -> Env -> [Code] -> Dump ->  Value
exec g s e (Ld (i,j):c) d = exec g (v:s) e c d where v = getLVar e i j
exec g s e (Ldc v:c) d = exec g (v:s) e c d
exec g s e (Ldg sym:c) d = exec g (v:s) e c d where v = getGVar g sym
exec g s e (Ldf code: c) d = exec g (Closure code e:s) e c d
exec g s e (Args n: c) d = exec g (List vs:s') e c d where vs = take n s
                                                           s' = drop n s
exec g (Closure code e':List vs:s) e (App:c) d = exec g [] (vs:e') code (Cont3 s e c:d)
exec g (v:s) e (Rtn:c) ((Cont3 s' e' c'):d) = exec g (v:s') e' c' d
exec g (VBool b:s) e (Sel ct cf:c) d = if b then exec g s e ct (Cont1 c:d)
                                       else exec g s e cf (Cont1 c:d)
exec g s e (Join:[]) (Cont1 c:d) = exec g s e c d
exec g (v:s) e (Pop:c) d = exec g s e c d
exec g (v:s) e (Def sym:c) d = exec g' (Sym sym:s) e c d where g' = (sym,v) : g
exec g (v:s) e (Stop:c) d =  v



