

module CompilerSpec where

import Compiler
import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)

-- helpers
l2 :: SExpr -> SExpr -> SExpr
l2 x y = (CELL x (CELL y NIL))

l3 :: SExpr -> SExpr -> SExpr -> SExpr
l3 x y z = (CELL x (CELL y (CELL z NIL)))

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL

spec :: Spec
spec = do
  describe "test case" $
    it "Literal" $
    compile (INT 1) `shouldBe` [Ldc (INT 1),Stop]

  describe "test case" $
    it "Literal" $
    compile (l3 (SYM "+") (INT 1) (INT 2)) `shouldBe` [Ldc (INT 1),
                                                       Ldc (INT 2),
                                                       Args 2,
                                                       Ldg "+",
                                                       App,
                                                       Stop]

  let lmd = lN[ SYM "lambda", lN [ SYM "n" ], SYM "n" ]

  describe "test case" $
    it "lambda" $ do 
    compile lmd `shouldBe` [ Ldf [ Ld (0,0), Rtn ], Stop ]

  describe "test case" $
    it "lambda apply" $ do 
    compile ( lN[ lmd , INT 1] ) `shouldBe` [ Ldc (INT 1),
                                              Args 1,
                                              Ldf [ Ld (0,0), Rtn ],
                                              App,
                                              Stop ]

  let lmd2 = lN[ SYM "lambda", lN [], INT 1, INT 2, INT 3, INT 4, INT 5 ]
  describe "test case" $
    it "lambda" $ do 
    compile lmd2 `shouldBe` [ Ldf [ Ldc (INT 1),
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
    compile ( lN[ lmd3 , INT 3, INT 5] ) `shouldBe` [ Ldc (INT 3),
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
    compile sexp `shouldBe` [ Ldc (BOOL True),
                              Sel [ Ldc (INT 1), Join ] [ Ldc (INT 2), Join ],
                              Stop ]

  describe "define" $
    it "simple int value" $ do
    let sexp = lN[ SYM "define", SYM "a", INT 2]
    compile sexp `shouldBe` [ Ldc (INT 2), Def "a", Stop ]


  describe "define" $
    it "lambda" $ do
    let sexp = lN[ SYM "define", SYM "a", lmd ]
    compile sexp `shouldBe` [ Ldf [ Ld (0,0), Rtn ],
                              Def "a",
                              Stop ]
      
  describe "quote" $
    it "lambda" $ do 
    compile (lN[ SYM "quote", SYM "a"] ) `shouldBe` [ Ldc (SYM "a"), Stop ]

  describe "quote" $
    it "lambda" $ do 
    compile (lN[ SYM "quote", lN[ SYM "a", SYM "b"] ] ) `shouldBe`
      [ Ldc (listToCell [ SYM "a",SYM "b" ]),Stop ]
      

