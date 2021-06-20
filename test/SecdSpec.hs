
module SecdSpec where

import Secd
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  
  describe "Stop" $
    it "case 1" $ 
    exec [] (Num 1:[]) [] (Stop:[]) [] `shouldBe` (Num 1)

  describe "Ld" $
    it "case 1" $ do
    let g = []    
    let s = []
    let e = [listToCell [Str "a",Str "b"]]
    let c = [Ld (0,1),Stop]
    let d = []
    exec g s e c d `shouldBe` (Str "b")

  describe "Ldc" $ do
    describe "String Literal" $
      it "case 1" $ do
      let g = []    
      let s = []
      let e = []
      let c = [Ldc (Str "b"),Stop]
      let d = []
      exec g s e c d `shouldBe` (Str "b")
  
    describe "Number Literal" $
      it "case 2" $ do
      let g = []    
      let s = []
      let e = []
      let c = [Ldc (Num 10),Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 10)


  describe "Ldg" $ do
    describe "String Literal" $
      it "case 1" $ do
      let g = [("a",Num 1),("b",Str "b")]
      let s = []
      let e = []
      let c = [Ldg "b",Stop]
      let d = []
      exec g s e c d `shouldBe` (Str "b")

  describe "Args" $ do
    describe "xxxx" $
      it "case 1" $ do
      let g = []
      let s = [Num 1, Num 2,Num 3,Num 4]
      let e = []
      let c = [Args 3,Stop]
      let d = []
      exec g s e c d `shouldBe` (listToCell [Num 1,Num 2,Num 3])

  let addFunc (Cell (Num x) (Cell (Num y) _)) = Num (x+y)

  describe "App" $ do
    describe "Primitive" $
      it "case 1" $ do
      let g = []
      let s = [Prim addFunc, listToCell [Num 3, Num 5], Str "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "Primitive2" $
      it "case 2" $ do
      let g = []
      let s = [listToCell [Num 3, Num 5], Str "xxx"]
      let e = []
      let c = [Ldc (Prim addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "Primitive3" $
      it "case 3" $ do
      let g = []
      let s = [Str "xxx"]
      let e = []
      let c = [Ldc (Num 3), Ldc (Num 5), Args 2, Ldc (Prim addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "Primitive4" $
      it "case 4" $ do
      let g = [("add", Prim addFunc)]
      let s = [Str "xxx"]
      let e = []
      let c = [Ldc (Num 3), Ldc (Num 5), Args 2, Ldg "add",App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "Closure1" $
      it "case 1" $ do
      let closEnv = [Nil]
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldc (Prim addFunc), App, Rtn ]
      let clos = Closure closCodes closEnv
      let g = []
      let s = [clos, listToCell [Num 3, Num 5], Str "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "Closure2" $
      it "case 2" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", App, Rtn ]
      let g = [("add", Prim addFunc)]
      let s = []
      let e = []
      let c = [Ldc (Num 3), Ldc (Num 5), Args 2, Ldf closCodes, App,Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 8)

    describe "UserFunction" $
      it "case 1" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", App, Rtn ]
      let g = [("add", Prim addFunc)]
      let s = []
      let e = []
      let c = [Ldf closCodes, Def "userAdd",
               Ldc (Num 3), Ldc (Num 5), Args 2, Ldg "userAdd", App, Stop ]
      let d = []
      exec g s e c d `shouldBe` (Num 8)


  describe "Pop" $ 
    it "case 1" $ do
    let g = []
    let s = [Num 1, Num 2, Num 3]
    let e = []
    let c = [Pop,Stop]
    let d = []
    exec g s e c d `shouldBe` (Num 2)
    
  describe "Def" $ 
    it "case 1" $ do
    let g = []
    let s = []
    let e = []
    let c = [Ldc (Num 2),Def "a", Ldg "a", Stop]
    let d = []
    exec g s e c d `shouldBe` (Num 2)

  describe "Sel&Join" $ do
    describe "then-block" $
      it "case 1" $ do
      let g = []
      let s = []
      let e = []
      let c = [Ldc (VBool True), Sel [Ldc (Num 1), Join] [Ldc (Num 2), Join], Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 1)

    describe "else-block" $
      it "case 2" $ do
      let g = []
      let s = []
      let e = []
      let c = [Ldc (VBool False), Sel [Ldc (Num 1), Join] [Ldc (Num 2), Join], Stop]
      let d = []
      exec g s e c d `shouldBe` (Num 2)

    
