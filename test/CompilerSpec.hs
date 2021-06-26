

module CompilerSpec where

import Compiler
import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified Data.HashTable.IO as H

-- helpers
l2 :: SExpr -> SExpr -> SExpr
l2 x y = (CELL x (CELL y NIL))

l3 :: SExpr -> SExpr -> SExpr -> SExpr
l3 x y z = (CELL x (CELL y (CELL z NIL)))

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL


shouldBeT ma b = do
  a <- runExceptT ma
  a `shouldBe` (Right b)

list' :: SecdFunc
list' _  e = return e

spec :: Spec
spec = do

  describe "test case" $
    it "Literal" $ do
    g <- H.new
    compile g (INT 1) `shouldBeT` [Ldc (INT 1),Stop]

  describe "test case" $
    it "Literal" $ do
    g <- H.new    
    compile g (l3 (SYM "+") (INT 1) (INT 2)) `shouldBeT` [Ldc (INT 1),
                                                          Ldc (INT 2),
                                                          Args 2,
                                                          Ldg "+",
                                                          App,
                                                          Stop]

  let lmd = lN[ SYM "lambda", lN [ SYM "n" ], SYM "n" ]

  describe "test case" $
    it "lambda" $ do
    g <- H.new    
    compile g lmd `shouldBeT` [ Ldf [ Ld (0,0), Rtn ], Stop ]

  describe "test case" $
    it "lambda apply" $ do 
    g <- H.new    
    compile g ( lN[ lmd , INT 1] ) `shouldBeT` [ Ldc (INT 1),
                                                 Args 1,
                                                 Ldf [ Ld (0,0), Rtn ],
                                                 App,
                                                 Stop ]
    
  let lmd2 = lN[ SYM "lambda", lN [], INT 1, INT 2, INT 3, INT 4, INT 5 ]
  describe "test case" $
    it "lambda" $ do 
    g <- H.new    
    compile g lmd2 `shouldBeT` [ Ldf [ Ldc (INT 1),
                                       Pop,
                                       Ldc (INT 2),
                                       Pop,
                                       Ldc (INT 3),
                                       Pop,
                                       Ldc (INT 4),
                                       Pop,
                                       Ldc (INT 5),
                                       Rtn ], Stop ]
  

  let lmd3 = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ], lN[ SYM "+", SYM "n", SYM "m" ] ]
  describe "test case" $
    it "lambda apply" $ do 
    g <- H.new    
    compile g ( lN[ lmd3 , INT 3, INT 5] ) `shouldBeT` [ Ldc (INT 3),
                                                         Ldc (INT 5),
                                                         Args 2,
                                                         Ldf [ Ld (0,0), Ld (0,1), Args 2,
                                                               Ldg "+",
                                                               App,
                                                               Rtn ],
                                                         App,
                                                         Stop ]
  describe "if" $
    it "lambda apply" $ do
    let sexp = lN[ SYM "if", BOOL True, INT 1, INT 2]
    g <- H.new    
    compile g sexp `shouldBeT` [ Ldc (BOOL True),
                                 Sel [ Ldc (INT 1), Join ] [ Ldc (INT 2), Join ],
                                 Stop ]

  describe "define" $
    it "simple int value" $ do
    let sexp = lN[ SYM "define", SYM "a", INT 2]
    g <- H.new    
    compile g sexp `shouldBeT` [ Ldc (INT 2), Def "a", Stop ]


  describe "define" $
    it "lambda" $ do
    let sexp = lN[ SYM "define", SYM "a", lmd ]
    g <- H.new    
    compile g sexp `shouldBeT` [ Ldf [ Ld (0,0), Rtn ],
                                 Def "a",
                                 Stop ]
      
  describe "quote" $
    it "lambda" $ do 
    g <- H.new    
    compile g (lN[ SYM "quote", SYM "a"] ) `shouldBeT` [ Ldc (SYM "a"), Stop ]

  describe "quote" $
    it "lambda" $ do 
    g <- H.new    
    compile g (lN[ SYM "quote", lN[ SYM "a", SYM "b"] ] ) `shouldBeT`
      [ Ldc (listToCell [ SYM "a",SYM "b" ]),Stop ]
      

  describe "define-macro" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    compile g sexp `shouldBeT` [ Ldf [ Ld (0,0), Rtn ],
                                 Defm "a",
                                 Stop ]
  describe "use macro" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", SYM "n"] ) `shouldBeT` [ Ldg "n", Stop ]

  describe "use macro2" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", INT 1] ) `shouldBeT` [ Ldc (INT 1), Stop ]


  describe "use macro3" $
    it "lambda" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ], SYM "m" ]
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", INT 1, INT 2] ) `shouldBeT` [ Ldc (INT 2), Stop ]

  describe "use macro4" $
    it "plus" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ],
                  lN [SYM "list", lN [ SYM "quote", SYM "+"], SYM "n", SYM "m" ] ]
    g <- H.fromList [("list", PRIM' list')]
    let sexp = lN[ SYM "define-macro", SYM "plus", lmd ]
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "plus", INT 1, INT 2] ) `shouldBeT` [Ldc (INT 1),
                                                            Ldc (INT 2),
                                                            Args 2,
                                                            Ldg "+", App, Stop ]

  describe "use macro5" $
    it "plus" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ],
                  lN [SYM "list", lN [ SYM "quote", SYM "+"], SYM "n", SYM "m" ] ]
    g <- H.fromList [("list", PRIM' list')]
    let sexp = lN[ SYM "define-macro", SYM "plus", lmd ]
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "plus", SYM "a", SYM "b"] ) `shouldBeT` [Ldg "a",
                                                                Ldg "b",
                                                                Args 2,
                                                                Ldg "+", App, Stop ]

