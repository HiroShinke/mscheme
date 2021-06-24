
module SecdSpec where

import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Expectations
import Control.Monad.Trans.Except

import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v



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
    let e = [listToCell [STR "a",STR "b"]]
    let c = [Ld (0,1),Stop]
    let d = []
    exec g s e c d `shouldBeT` STR "b"

  describe "Ldc" $ do
    describe "String Literal" $
      it "case 1" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (STR "b"),Stop]
      let d = []
      exec g s e c d `shouldBeT` (STR "b")
  
    describe "Number Literal" $
      it "case 2" $ do
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
      exec g s e c d `shouldBeT` (listToCell [INT 1,INT 2,INT 3])

  let addFunc _ (CELL (INT x) (CELL (INT y) _)) = return $ INT (x+y)

  describe "App" $ do
    describe "PRIMitive" $
      it "case 1" $ do
      g <- H.new
      let s = [PRIM addFunc, listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "PRIMitive2" $
      it "case 2" $ do
      g <- H.new
      let s = [listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [Ldc (PRIM addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "PRIMitive3" $
      it "case 3" $ do
      g <- H.new
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldc (PRIM addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "PRIMitive4" $
      it "case 4" $ do
      g <- H.fromList [("add", PRIM addFunc)]
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldg "add",App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure1" $
      it "case 1" $ do
      let closEnv = [NIL]
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldc (PRIM addFunc), App, Rtn ]
      let clos = CLOS' closCodes closEnv
      g <- H.new
      let s = [clos, listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure2" $
      it "case 2" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", App, Rtn ]
      g <- H.fromList [("add", PRIM addFunc)]
      let s = []
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldf closCodes, App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "UserFunction" $
      it "case 1" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", App, Rtn ]
      g <- H.fromList [("add", PRIM addFunc)]
      let s = []
      let e = []
      let c = [Ldf closCodes, Def "userAdd",
               Ldc (INT 3), Ldc (INT 5), Args 2, Ldg "userAdd", App, Stop ]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)


  describe "Pop" $ 
    it "case 1" $ do
    g <- H.new
    let s = [INT 1, INT 2, INT 3]
    let e = []
    let c = [Pop,Stop]
    let d = []
    exec g s e c d `shouldBeT` (INT 2)
    
  describe "Def" $ 
    it "case 1" $ do
    g <- H.new
    let s = []
    let e = []
    let c = [Ldc (INT 2),Def "a", Ldg "a", Stop]
    let d = []
    exec g s e c d `shouldBeT` (INT 2)

  describe "Sel&Join" $ do
    describe "then-block" $
      it "case 1" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (BOOL True), Sel [Ldc (INT 1), Join] [Ldc (INT 2), Join], Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 1)

    describe "else-block" $
      it "case 2" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (BOOL False), Sel [Ldc (INT 1), Join] [Ldc (INT 2), Join], Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 2)

    
