

module ReaderSpec where

import Reader
import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified Data.HashTable.IO as H
import qualified SecdFuncs as F

-- helpers

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL

shouldBeEvaluated s v = do
  case readSExpr s of
    Left  (ParseErr xs' "EOF") -> return ()
    Left  (ParseErr xs' mes)   -> putStrLn mes
    Right (expr, xs') -> expr `shouldBe` v

spec :: Spec
spec = do
  describe "simple atom1" $ do
    it "integer" $ "1" `shouldBeEvaluated` (INT 1)
    it "symbol" $ "a" `shouldBeEvaluated` (SYM "a")
    it "nil" $ "()" `shouldBeEvaluated` NIL
    it "list" $ "(a b c)" `shouldBeEvaluated` lN [SYM "a",SYM "b",SYM "c"]
    it "list with dot1" $ "(a . b)" `shouldBeEvaluated` (CELL (SYM "a")
                                                         (SYM "b"))
    it "list with dot2" $ "(a b . c)" `shouldBeEvaluated` (CELL (SYM "a")
                                                           (CELL (SYM "b") (SYM "c")))
  describe "lambda" $ do
    it "normal" $ "(lambda (n) n)" `shouldBeEvaluated` lN[ SYM "lambda",
                                                           lN[ SYM "n" ],
                                                           SYM "n" ]
    it "list parmater1" $ "(lambda n n)" `shouldBeEvaluated` lN[ SYM "lambda",
                                                                 SYM "n",
                                                                 SYM "n" ]
    it "list parmater2" $ "(lambda (n.m) n)" `shouldBeEvaluated` lN[ SYM "lambda",
                                                                     (CELL (SYM "n") (SYM "m")),
                                                                     SYM "n" ]

    
