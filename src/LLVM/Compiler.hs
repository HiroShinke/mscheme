{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVM.Compiler where

import qualified  Data.Text.Lazy as T
import Data.Text.Internal.Lazy
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Fix
import SExpr
import Error

import Data.Functor.Identity

import LLVM.Pretty
import LLVM.AST hiding (function, value)
import LLVM.AST.Type as AST
import LLVM.AST.Constant as C
import LLVM.AST.IntegerPredicate as P
import LLVM.AST.AddrSpace
import LLVM.AST.Name

import LLVM.IRBuilder.Module
import LLVM.IRBuilder.Monad
import LLVM.IRBuilder.Instruction
import LLVM.IRBuilder.Constant
import Data.List as L
import qualified Data.Map as Map
import Data.String
-- import qualified Data.ByteString.Short as B

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)
type Binds = Map.Map String Operand
-- type Func = Expr -> StateT Binds (IRBuilderT ModuleBuilder) Operand

cellToList :: SExpr -> [SExpr]
cellToList (CELL x xs) = x : cellToList xs
cellToList  NIL        = []

toName :: SExpr -> String
toName (SYM n) = n
toName _       = error "not symbol"

--- compile

compile :: [SExpr] -> String
compile exprs = T.unpack $ ppllvm $ buildModule "main" $ mdo
  let (defs,rest) = L.partition isDefine exprs
  form <- globalStringPtr "%d\n" "putNumForm"
  printf <- externVarArgs "printf" [ptr i8] i32
  let binds = Map.fromList []
  flip runStateT binds $ mapM compT defs
  function "main" [] i32 $ \[] -> mdo
    (rs,_) <- flip runStateT binds $ mapM comp rest
    let n = length rs    
    call printf [(ConstantOperand form, []), (rs!!(n-1), [])]
    ret (int32 0)
  function "showInt" [(i32,"n")] i32 $ \[n] -> mdo
    call printf [(ConstantOperand form, []), (n, [])]
    ret (int32 0)
   where
    isDefine :: SExpr -> Bool
    isDefine (CELL (SYM "define") _) = True
    isDefine _                       = False

comp :: (MonadFix m,MonadIRBuilder m) =>  SExpr -> StateT Binds m Operand
comp v@(INT n) = return (int32 n)
comp v@(SYM name) = mdo
  binds <- get
  case binds Map.!? name of
    Just x -> pure x
    Nothing -> error $ "'" <> name <> "' doesn't exist in scope"
comp (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) = mdo
  pred' <- comp pred 
  condBr pred' tb' eb'
  tb' <- block `named` "thenB"
  tr <- comp tb
  br mb
  eb' <- block `named` "elseB"
  er <- comp eb
  br mb
  mb <-  block `named` "mergeB"
  phi [(tr,tb'),(er,eb')]

  
comp (CELL func@(SYM sym) args) = mdo
  let es = cellToList args
  let n  = length es
  es' <- mapM comp es
  case sym of
    "+" -> add (es'!!0) (es'!!1)
    "-" -> sub (es'!!0) (es'!!1)
    "*" -> mul (es'!!0) (es'!!1)
    "/" -> sdiv (es'!!0) (es'!!1)
    "="  -> icmp P.EQ (es'!!0) (es'!!1)
    "!=" -> icmp P.NE (es'!!0) (es'!!1)
    ">"  -> icmp P.UGT (es'!!0) (es'!!1)
    "<"  -> icmp P.ULT (es'!!0) (es'!!1)
    _   -> mdo
      let typ = FunctionType i32 (replicate n i32) False
      let ptrTyp = AST.PointerType typ (AddrSpace 0)
      let ref = GlobalReference ptrTyp (mkName sym)
      call (ConstantOperand ref) (zip es' (repeat []))

comp e = error $ "error :e=" ++ (show e)

compBody es = mapM comp (cellToList es)
  
compT :: (MonadFix m,MonadModuleBuilder m) => SExpr -> StateT Binds m Operand
compT (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) = compT' n e
compT' nameStr (CELL (SYM "lambda") (CELL args body)) = mdo
  let n = fromString nameStr
  env <- get  
  function n params i32 $ \ops -> mdo
    let env' = insertNVList env (zip argNames ops)
    (rs,_) <- flip runStateT env' $ compBody body
    let m = length rs
    ret $ rs!!(m-1)
  where argNames = Prelude.map toName (cellToList args)
        params = zip (repeat i32) (Prelude.map (ParameterName . fromString) argNames)
        insertNVList env ((n,v):xs) = let env' = insertNVList env xs
                                      in  Map.insert n v env'
        insertNVList env []         = env
-- compT' nameStr e = mdo
--    let n = fromString nameStr
--    env <- get    
--    v <- comp e
--    let env' = Map.insert n v env
--    put env'
--    return v
  
