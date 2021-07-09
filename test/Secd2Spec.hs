
module Secd2Spec where

import Mutable.Secd
import Mutable.SExpr
import Data.IORef
import System.IO.Unsafe
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified SecdFuncs as F
import qualified SExpr as I
import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

lN :: [SExpr] -> SExpr
lN = unsafePerformIO . listToCell

shouldBeT m v = do
  v' <- runExceptT m
  v' `shouldBe` (Right v)

spec :: Spec
spec = do
  
  describe "Stop" $
    it "case 1" $ do
    g <- H.new
    exec g (INT 1:[]) [] (Stop:[]) [] `shouldBeT` (INT 1)

  describe "Ld" $
    it "case 1" $ do
    g <- H.new
    let s = []
    f' <- newIORef $ lN[STR "a",STR "b"]
    let e = [f'] 
    let c = [Ld (0,1),Stop]
    let d = []
    exec g s e c d `shouldBeT` STR "b"

  describe "Ldc" $ do
    it "String Literal" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (STR "b"),Stop]
      let d = []
      exec g s e c d `shouldBeT` (STR "b")
  
    it "Number Literal" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (INT 10),Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 10)

  describe "Ldg" $ do
    describe "String Literal" $
      it "case 1" $ do
      g <- H.fromList [("a",INT 1),("b",STR "b")]
      let s = []
      let e = []
      let c = [Ldg "b",Stop]
      let d = []
      exec g s e c d `shouldBeT` (STR "b")

  describe "Args" $ do
    describe "xxxx" $
      it "case 1" $ do
      g <- H.new
      let s = [INT 1, INT 2,INT 3,INT 4]
      let e = []
      let c = [Args 3,Stop]
      let d = []
      cell <- listToCell [INT 3,INT 2,INT 1]
      exec g s e c d `shouldBeT` cell


  let addFunc _ e = do
        l <- liftIO $ cellToList e
        case l of
          (INT x: INT y: _ ) -> return $ INT (x+y)
          _                  -> error "error"
  
  describe "App" $ do
    it "Primitive 1" $ do
      g <- H.new
      let s = [PRIM' addFunc, lN[INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)


  


      
