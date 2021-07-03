

module Repl3Spec where

import System.IO
import qualified Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Reader
import Compiler
import Error
import SExpr
import SecdFuncs

import Test.Hspec (Spec, describe, it, shouldBe)

initGEnv :: [(String, SExpr)]
initGEnv = [("true",   true),
            ("false",  false),
            ("eq?",    PRIM' eq'),
            ("equal?", PRIM' equal'),
            ("pair?",  PRIM' pair),
            ("+",      PRIM' adds),
            ("-",      PRIM' subs),
            ("*",      PRIM' muls),
            ("/",      PRIM' divs),
            ("mod",    PRIM' mod'),
            ("=",      PRIM' eqNum),
            ("<",      PRIM' ltNum),
            (">",      PRIM' gtNum),
            ("<=",     PRIM' ltEq),
            (">=",     PRIM' gtEq),
            ("car",    PRIM' car),
            ("cdr",    PRIM' cdr),
            ("cons",   PRIM' cons),
            ("list",   PRIM' list'),
            ("append", PRIM' append'),
            ("error",  PRIM' error')
           ]


shouldBeEvaluated s v = do
  g <- H.fromList initGEnv
  x <- runExceptT $ iter g s []
  case x of
    Right (v':_) -> 
      v' `shouldBe` v
    Left e -> liftIO $ putStrLn (show e)
  where
    iter :: GEnv -> String -> [SExpr] -> Scm [SExpr]
    iter g xs accm = 
      case readSExpr xs of
        Left  (ParseErr xs' "EOF") -> return accm
        Left  (ParseErr xs' mes)   -> do
          liftIO $ putStrLn mes
          return []
        Right (expr, xs') -> do code <- compile g expr
                                v <- S.exec g [] [] code [] 
                                iter g xs' (v:accm)

spec :: Spec
spec = do
  describe "Basics" $ do
      it "define" $ do
        "(define a 1)" `shouldBeEvaluated` (SYM "a")
      it "define" $ do
        "(define a 1) a" `shouldBeEvaluated` (INT 1)

  describe "Closures" $ do
      it "lambda" $ do
        "((lambda (n) n) 1)" `shouldBeEvaluated` (INT 1)
      it "lambda2" $ do
        "((lambda (n m) (+ n m)) 1 2)" `shouldBeEvaluated` (INT 3)
      it "lambda3" $ do
        "((lambda (n m) (- n m)) 1 2)" `shouldBeEvaluated` (INT (-1))

  describe "function definition" $ do
      it "id" $ 
        "(define id (lambda (n) n)) (id 1)" `shouldBeEvaluated` (INT 1)
      it "plus" $
        ( "(define plus (lambda (n m) (+ n m))) " ++
          "(plus 1 2)" ) `shouldBeEvaluated` (INT 3)
      it "minus" $
        ( "(define minus (lambda (n m) (- n m))) " ++
          "(minus 1 2)" ) `shouldBeEvaluated` (INT (-1))
      it "perm1" $
        "(define perm1\
\                (lambda (n) (if (= n 0)\
\                                 1\
\                                 (* n (perm1 (- n 1))))))\
\        (perm1 10)" `shouldBeEvaluated` (INT 3628800)
      it "perm2" $
        "(define perm2\
\                (lambda (n a) (if (= n 0)\
\                                  a\
\                                 (perm2 (- n 1) (* n a) ))))\
\        (perm2 10 1)" `shouldBeEvaluated` (INT 3628800)


  describe "macro definition" $ do
      it "id" $ 
        "(define-macro id (lambda (n) n)) (id 1)" `shouldBeEvaluated` (INT 1)
      it "plus" $
        ( "(define-macro plus (lambda (n m) (+ n m))) " ++
          "(plus 1 2)" ) `shouldBeEvaluated` (INT 3)
      it "minus" $
        ( "(define-macro minus (lambda (n m) (- n m))) " ++
          "(minus 1 2)" ) `shouldBeEvaluated` (INT (-1))
        






