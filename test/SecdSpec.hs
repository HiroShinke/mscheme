
module SecdSpec where

import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified SecdFuncs as F
import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL

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
      exec g s e c d `shouldBeT` (listToCell [INT 3,INT 2,INT 1])

  let addFunc _ (CELL (INT x) (CELL (INT y) _)) = return $ INT (x+y)

  describe "App" $ do
    describe "Primitive" $
      it "case 1" $ do
      g <- H.new
      let s = [PRIM' addFunc, listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive2" $
      it "case 2" $ do
      g <- H.new
      let s = [listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [Ldc (PRIM' addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive3" $
      it "case 3" $ do
      g <- H.new
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldc (PRIM' addFunc),App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive4" $
      it "case 4" $ do
      g <- H.fromList [("add", PRIM' addFunc)]
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldg "add",App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure1" $
      it "case 1" $ do
      let closEnv = [NIL]
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldc (PRIM' addFunc), App, Rtn ]
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
      g <- H.fromList [("add", PRIM' addFunc)]
      let s = []
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldf closCodes, App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure3" $ do
      it "list parameter 1" $ do
        let closCodes = [Ld (0,-1), Rtn ]
        let s = []
        let e = []
        let c = [Ldc (INT 1), Ldc (INT 2), Ldc (INT 3),
                 Args 3, Ldf closCodes, App, Stop]
        let d = []
        g <- H.new
        exec g s e c d `shouldBeT` lN[INT 1, INT 2, INT 3]

      it "list parameter 2" $ do
        let closCodes = [Ld (0,-2), Rtn ]
        let s = []
        let e = []
        let c = [Ldc (INT 1), Ldc (INT 2), Ldc (INT 3),
                 Args 3, Ldf closCodes, App, Stop]
        let d = []
        g <- H.new
        exec g s e c d `shouldBeT` lN[INT 2, INT 3]

      it "list parameter 3" $ do
        let closCodes = [Ld (0,-3), Rtn ]
        let s = []
        let e = []
        let c = [Ldc (INT 1), Ldc (INT 2), Ldc (INT 3),
                 Args 3, Ldf closCodes, App, Stop]
        let d = []
        g <- H.new
        exec g s e c d `shouldBeT` lN[INT 3]

    describe "UserFunction" $
      it "case 1" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", App, Rtn ]
      g <- H.fromList [("add", PRIM' addFunc)]
      let s = []
      let e = []
      let c = [Ldf closCodes, Def "userAdd",
               Ldc (INT 3), Ldc (INT 5), Args 2, Ldg "userAdd", App, Stop ]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Macro" $
      it "case 1" $ do
      g <- H.new
      let closCodes = [Ld (0,0), Rtn ]
      let s = []
      let e = []
      let c = [Ldf closCodes, Defm "a", Stop]
      let d = []
      exec g s e c d `shouldBeT` (SYM "a")

  describe "TApp" $ do
    describe "Primitive" $
      it "same as App for primitives 1" $ do
      g <- H.new
      let s = [PRIM' addFunc, listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [TApp,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive2" $
      it "same as App for primitives 2" $ do
      g <- H.new
      let s = [listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [Ldc (PRIM' addFunc),TApp,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive3" $
      it "same as App for primitives 3" $ do
      g <- H.new
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldc (PRIM' addFunc),TApp,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Primitive4" $
      it "same as App for primitives 4" $ do
      g <- H.fromList [("add", PRIM' addFunc)]
      let s = [STR "xxx"]
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldg "add",TApp,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure1" $
      it "case 1" $ do
      let closEnv = [NIL]
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldc (PRIM' addFunc), TApp, Rtn ]
      let clos = CLOS' closCodes closEnv
      g <- H.new
      let s = [clos, listToCell [INT 3, INT 5], STR "xxx"]
      let e = []
      let c = [App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "Closure2" $
      it "case 2" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", TApp, Rtn ]
      g <- H.fromList [("add", PRIM' addFunc)]
      let s = []
      let e = []
      let c = [Ldc (INT 3), Ldc (INT 5), Args 2, Ldf closCodes, App,Stop]
      let d = []
      exec g s e c d `shouldBeT` (INT 8)

    describe "UserFunction" $
      it "case 1" $ do
      let closCodes = [Ld (0,0), Ld (0,1), Args 2, Ldg "add", TApp, Rtn ]
      g <- H.fromList [("add", PRIM' addFunc)]
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

  describe "Selr&Rtn" $ do
    describe "then-block" $
      it "case 1" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (BOOL True), Selr [Ldc (INT 1), Rtn] [Ldc (INT 2), Rtn], Rtn]
      let d = [Cont3 [] [] [Stop]]
      exec g s e c d `shouldBeT` (INT 1)

    describe "else-block" $
      it "case 2" $ do
      g <- H.new
      let s = []
      let e = []
      let c = [Ldc (BOOL False), Selr [Ldc (INT 1), Rtn] [Ldc (INT 2), Rtn], Rtn]
      let d = [Cont3 [] [] [Stop]]
      exec g s e c d `shouldBeT` (INT 2)

    let cscont = [Ldc (SYM "c"), Args 3, Ldg "list", App, Stop]
    let cs = [Ldc (SYM "a"), Ldct cscont, Args 1,
              Ldf [Ld (0,0), Def "x", Pop, Ldc (SYM "b"), Rtn], App] ++ cscont

    describe "call/cc" $
      it "save continuation" $ do
      g <- H.fromList [("list",PRIM' F.list')]
      let s = []
      let e = []
      let c = cs
      let d = []
      exec g s e c d `shouldBeT` lN[ SYM "a", SYM "b", SYM "c" ]

    describe "call/cc" $
      it "save continuation" $ do
      g <- H.fromList [("list",PRIM' F.list')]
      let s = []
      let e = []
      let c = cs
      let d = []
      runExceptT $ exec g s e c d
      let cs' = [ Ldc (SYM "d"), Args 1, Ldg "x", App ]
      exec g [] [] cs' [] `shouldBeT` lN[ SYM "a", SYM "d", SYM "c" ]
      
