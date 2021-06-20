

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
  describe "test case" $
    it "lambda" $ do 
    let lmd = lN[ SYM "labmda", lN [ SYM "n" ], SYM "n" ]
    compile ( lN[ lmd , INT 1] ) `shouldBe` [ S.Ldc (S.Num 1),
                                              S.Args 1,
                                              S.Ldf [ S.Ld (0,0), S.Rtn ],
                                              S.App,
                                              S.Stop ]
