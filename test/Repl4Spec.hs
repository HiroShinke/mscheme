

module Repl4Spec where

import System.IO
import System.IO.Unsafe
import Data.IORef
import qualified Mutable.Secd as S
import qualified Data.HashTable.IO as H
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Reader
import Mutable.Compiler
import Error 
import SExpr hiding (debugPrint)
import qualified Mutable.SExpr as M
import Mutable.SecdFuncs
import Test.Hspec (Spec, describe, it, shouldBe)

initGEnv :: [(String, M.SExpr)]
initGEnv = [("true",   M.true),
            ("false",  M.false),
            ("eq?",    M.PRIM' eq'),
            ("equal?", M.PRIM' equal'),
            ("pair?",  M.PRIM' pair),
            ("+",      M.PRIM' adds),
            ("-",      M.PRIM' subs),
            ("*",      M.PRIM' muls),
            ("/",      M.PRIM' divs),
            ("mod",    M.PRIM' mod'),
            ("=",      M.PRIM' eqNum),
            ("<",      M.PRIM' ltNum),
            (">",      M.PRIM' gtNum),
            ("<=",     M.PRIM' ltEq),
            (">=",     M.PRIM' gtEq),
            ("car",    M.PRIM' car),
            ("cdr",    M.PRIM' cdr),
            ("cons",   M.PRIM' cons),
            ("list",   M.PRIM' list'),
            ("append", M.PRIM' append'),
            ("error",  M.PRIM' error')
           ]

debugPrint s = if False
               then liftIO $ putStrLn s
               else return ()

shouldBeEvaluated s v = do
  g <- H.fromList initGEnv
  x <- runExceptT $ iter g s []
  case x of
    Right (v':_) -> 
      v' `shouldBe` v
    Left e -> debugPrint (show e)
  where
    iter :: M.GEnv -> String -> [M.SExpr] -> Scm [M.SExpr]
    iter g xs accm = 
      case readSExpr xs of
        Left  (ParseErr xs' "EOF") -> return accm
        Left  (ParseErr xs' mes)   -> do
          liftIO $ putStrLn mes
          return []
        Right (expr, xs') -> do debugPrint (show expr)
                                code <- compile g expr
                                debugPrint (show code)
                                v <- S.exec g [] [] code [] 
                                iter g xs' (v:accm)

lN :: [M.SExpr] -> M.SExpr
lN = unsafePerformIO . M.listToCell

consM :: M.SExpr -> M.SExpr -> M.SExpr
consM x xs = unsafePerformIO $
  M.CELL <$> newIORef x <*> newIORef xs

spec :: Spec
spec = do
  describe "Basics" $ do
      it "define" $ do
        "(define a 1)" `shouldBeEvaluated` (M.SYM "a")
      it "define" $ do
        "(define a 1) a" `shouldBeEvaluated` (M.INT 1)

  describe "Closures" $ do
      it "lambda" $ do
        "((lambda (n) n) 1)" `shouldBeEvaluated` (M.INT 1)
      it "lambda2" $ do
        "((lambda (n m) (+ n m)) 1 2)" `shouldBeEvaluated` (M.INT 3)
      it "lambda3" $ do
        "((lambda (n m) (- n m)) 1 2)" `shouldBeEvaluated` (M.INT (-1))

  describe "function definition" $ do
      it "id" $ 
        "(define id (lambda (n) n)) (id 1)" `shouldBeEvaluated` (M.INT 1)
      it "plus" $
        ( "(define plus (lambda (n m) (+ n m))) " ++
          "(plus 1 2)" ) `shouldBeEvaluated` (M.INT 3)
      it "minus" $
        ( "(define minus (lambda (n m) (- n m))) " ++
          "(minus 1 2)" ) `shouldBeEvaluated` (M.INT (-1))
      it "perm1" $
        "(define perm1\
\                (lambda (n) (if (= n 0)\
\                                 1\
\                                 (* n (perm1 (- n 1))))))\
\        (perm1 10)" `shouldBeEvaluated` (M.INT 3628800)
      it "perm2" $
        "(define perm2\
\                (lambda (n a) (if (= n 0)\
\                                  a\
\                                 (perm2 (- n 1) (* n a) ))))\
\        (perm2 10 1)" `shouldBeEvaluated` (M.INT 3628800)

  describe "function definition" $ do
      it "list parameter1" $
        "(define id (lambda n n)) (id 'a 'b 'c)" `shouldBeEvaluated` lN[ M.SYM "a",
                                                                          M.SYM "b",
                                                                          M.SYM "c" ] 
      it "list parameter2" $
        "(define id (lambda (n . m) m)) (id 'a 'b 'c)" `shouldBeEvaluated` lN[ M.SYM "b",
                                                                                M.SYM "c" ]
      it "list parameter3" $
        "(define id (lambda (l m . n) n)) (id 'a 'b 'c)" `shouldBeEvaluated` lN[ M.SYM "c" ]

  describe "macro definition" $ do
      it "id" $ 
        "(define-macro id (lambda (n) n)) (id 1)" `shouldBeEvaluated` (M.INT 1)
      it "plus" $
        ( "(define-macro plus (lambda (n m) (+ n m))) " ++
          "(plus 1 2)" ) `shouldBeEvaluated` (M.INT 3)
      it "minus" $
        ( "(define-macro minus (lambda (n m) (- n m))) " ++
          "(minus 1 2)" ) `shouldBeEvaluated` (M.INT (-1))
        
  describe "primitives" $ do
      it "eq1" $ 
        "(eq? 1 2)" `shouldBeEvaluated` (M.BOOL False)
      it "eq2" $ 
        "(eq? 1 1)" `shouldBeEvaluated` (M.BOOL True)
      it "eq3" $ 
        "(eq? 'a 'a)" `shouldBeEvaluated` (M.BOOL True)
      it "eq4" $ 
        "(eq? '(a b) '(a b))" `shouldBeEvaluated` (M.BOOL True)
      it "pair?1" $ 
        "(pair? 'a)" `shouldBeEvaluated` (M.BOOL False)
      it "pair?2" $ 
        "(pair? '(a b))" `shouldBeEvaluated` (M.BOOL True)
      it "pair?3" $ 
        "(pair? '(a . b))" `shouldBeEvaluated` (M.BOOL True)
      it "car1" $ 
        "(car '(a b))" `shouldBeEvaluated` (M.SYM "a")
      it "cdr1" $ 
        "(cdr '(a b))" `shouldBeEvaluated` lN[ M.SYM "b" ]
      it "cons1" $ 
        "(cons 'a '(b c))" `shouldBeEvaluated` lN[ M.SYM "a", M.SYM "b", M.SYM "c" ]
      it "cons2" $ 
        "(cons 'a 'b)" `shouldBeEvaluated` (consM (M.SYM "a") (M.SYM "b"))

  describe "set!" $ do
      it "set1" $
         "( (lambda (n m) (set! m 1) (list n m) ) 1 2) " `shouldBeEvaluated` lN[ M.INT 1,
                                                                                 M.INT 1]
      it "set2" $
        "( (lambda (n m) \
\               ((lambda (z) (set! m z)) n)\
\               (list n m)\
\              )\
\            1 2) " `shouldBeEvaluated` lN[ M.INT 1,M.INT 1]

      it "set3" $
       "( (lambda (l m n) \
\               ((lambda (l m)\
\                   ((lambda (l)\
\                     (set! l 10)\
\                     (set! m 11)\
\                     (set! n 12))\
\                     l)\
\                 )\
\                l m)\
\         (list l m n)\
\         )\
\         1 2 3)" `shouldBeEvaluated` lN[ M.INT 1,M.INT 2,M.INT 12]
        
      it "set4" $
        "(define a 10) \
\        ((lambda (n m) \
\               ((lambda (z) (set! a z)) n)\
\               (list n m)\
\              )\
\            1 2)\
\         a" `shouldBeEvaluated` (M.INT 1)
                                                                                 




         
  





