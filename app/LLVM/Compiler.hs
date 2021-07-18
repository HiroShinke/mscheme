{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module LLVM.Compiler where

import qualified  Data.Text.Lazy as T
import Data.Text.Internal.Lazy
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
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
import Data.Map as Map
import Data.String
-- import qualified Data.ByteString.Short as B

type LLVMBuilder = IRBuilderT (ModuleBuilderT Identity)
type Binds = Map.Map String Operand
-- type Func = Expr -> ReaderT Binds (IRBuilderT ModuleBuilder) Operand

cellToList :: SExpr -> [SExpr]
cellToList (CELL x xs) = x : cellToList xs
cellToList  NIL        = []

toName :: SExpr -> String
toName (SYM n) = n
toName _       = error "not symbol"

--- compile

compile :: [SExpr] -> String
compile exprs = T.unpack $ ppllvm $ buildModule "main" $ mdo
  form <- globalStringPtr "%d\n" "putNumForm"
  printf <- externVarArgs "printf" [ptr i8] i32
  rs <- mapM compT exprs
  function "main" [] i32 $ \[] -> mdo
    let n = length rs
    r <- call (rs!!(n-1)) []
    call printf [(ConstantOperand form, []), (r, [])]
    ret (int32 0)

comp :: SExpr -> ReaderT Binds (IRBuilderT (ModuleBuilderT Identity)) Operand
comp v@(INT n) = return (int32 n)
comp v@(SYM name) = mdo
  binds <- ask
  case binds Map.!? name of
    Just x -> pure x
    Nothing -> error $ "'" <> name <> "' doesn't exist in scope"
comp (CELL (SYM "if") (CELL pred (CELL tb (CELL eb NIL) ))) = mdo
  pred' <- comp pred 
  cnd <- icmp P.EQ (ConstantOperand (C.Int 32 0)) pred'
  condBr cnd tb' eb'
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
    _   -> mdo
      let typ = FunctionType i32 (replicate n i32) False
      let ptrTyp = AST.PointerType typ (AddrSpace 0)
      let ref = GlobalReference ptrTyp (mkName sym)
      call (ConstantOperand ref) (zip es' (repeat []))
comp e = error $ "error :e=" ++ (show e)

compBody es = mapM comp (cellToList es)
  
compT :: SExpr -> (ModuleBuilderT Identity) Operand
compT (CELL (SYM "define") (CELL (SYM n) (CELL e NIL))) = compT' n e
compT' nameStr (CELL (SYM "lambda") (CELL args body)) = mdo
  let n = fromString nameStr
  function n params i32 $ \ops -> mdo
    let binds = Map.fromList (zip argNams ops)
    rs <- flip runReaderT binds $ compBody body
    let m = length rs
    ret $ rs!!(m-1)
  where argNams = Prelude.map toName (cellToList args)
        params = zip (repeat i32) (Prelude.map (ParameterName . fromString) argNams)


