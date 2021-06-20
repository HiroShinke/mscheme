

module CompilerSpec where

import Compiler
import Secd as S
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
    compile (INT 1) `shouldBe` [S.Ldc (S.Num 1),S.Stop]

  describe "test case" $
    it "Literal" $
    compile (l3 (SYM "+") (INT 1) (INT 2)) `shouldBe` [S.Ldc (S.Num 1),
                                                        S.Ldc (S.Num 2),
                                                        S.Args 2,
                                                        S.Ldg "+",
                                                        S.App,
                                                        S.Stop]

  let lmd = lN[ SYM "lambda", lN [ SYM "n" ], SYM "n" ]

  describe "test case" $
    it "lambda" $ do 
    compile lmd `shouldBe` [ S.Ldf [ S.Ld (0,0), S.Rtn ], S.Stop ]

  describe "test case" $
    it "lambda apply" $ do 
    compile ( lN[ lmd , INT 1] ) `shouldBe` [ S.Ldc (S.Num 1),
                                              S.Args 1,
                                              S.Ldf [ S.Ld (0,0), S.Rtn ],
                                              S.App,
                                              S.Stop ]

  let lmd2 = lN[ SYM "lambda", lN [], INT 1, INT 2, INT 3, INT 4, INT 5 ]
  describe "test case" $
    it "lambda" $ do 
    compile lmd2 `shouldBe` [ S.Ldf [ S.Ldc (S.Num 1),
                                      S.Pop,
                                      S.Ldc (S.Num 2),
                                      S.Pop,
                                      S.Ldc (S.Num 3),
                                      S.Pop,
                                      S.Ldc (S.Num 4),
                                      S.Pop,
                                      S.Ldc (S.Num 5),
                                      S.Rtn ], S.Stop ]


  let lmd3 = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ], lN[ SYM "+", SYM "n", SYM "m" ] ]
  describe "test case" $
    it "lambda apply" $ do 
    compile ( lN[ lmd3 , INT 3, INT 5] ) `shouldBe` [ S.Ldc (S.Num 3),
                                                      S.Ldc (S.Num 5),
                                                      S.Args 2,
                                                      S.Ldf [ S.Ld (0,0), S.Ld (0,1), S.Args 2,
                                                              S.Ldg "+",
                                                              S.App,
                                                              S.Rtn ],
                                                      S.App,
                                                      S.Stop ]
  describe "if" $
    it "lambda apply" $ do
    let sexp = lN[ SYM "if", BOOL True, INT 1, INT 2]
    compile sexp `shouldBe` [ S.Ldc (S.VBool True),
                              S.Sel [ S.Ldc (S.Num 1), S.Join ] [ S.Ldc (S.Num 2), S.Join ],
                              S.Stop ]

  describe "define" $
    it "simple int value" $ do
    let sexp = lN[ SYM "define", SYM "a", INT 2]
    compile sexp `shouldBe` [ S.Ldc (S.Num 2), S.Def "a", S.Stop ]


  describe "define" $
    it "lambda" $ do
    let sexp = lN[ SYM "define", SYM "a", lmd ]
    compile sexp `shouldBe` [ S.Ldf [ S.Ld (0,0), S.Rtn ],
                              S.Def "a",
                              S.Stop ]
      
  describe "quote" $
    it "lambda" $ do 
    compile (lN[ SYM "quote", SYM "a"] ) `shouldBe` [ S.Ldc (S.Sym "a"), S.Stop ]

  describe "quote" $
    it "lambda" $ do 
    compile (lN[ SYM "quote", lN[ SYM "a", SYM "b"] ] ) `shouldBe`
      [ S.Ldc (S.listToCell [ S.Sym "a",S.Sym "b" ]),S.Stop ]
      

