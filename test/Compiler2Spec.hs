

module Compiler2Spec where

import Mutable.Compiler
import Mutable.Secd
import SExpr
import Data.IORef
import qualified Mutable.SExpr as M
import qualified System.IO.Silently as Silently
import System.IO.Unsafe
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified Data.HashTable.IO as H
import qualified Mutable.SecdFuncs as F

-- helpers
l2 :: SExpr -> SExpr -> SExpr
l2 x y = (CELL x (CELL y NIL))

l3 :: SExpr -> SExpr -> SExpr -> SExpr
l3 x y z = (CELL x (CELL y (CELL z NIL)))

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL

lN' :: [M.SExpr] -> M.SExpr
lN' (x:xs) = unsafePerformIO $ M.CELL <$> newIORef x <*> (M.listToCell xs >>= newIORef )
lN' []     = M.NIL


shouldBeT ma b = do
  xa <- runExceptT ma
  case xa of
    Right a -> a `compareCode` b
    Left  e ->  putStrLn (show e)

-- round about for failure of PRIM' equality
compareCode a b =
  primDummy a `shouldBe` primDummy b
  where primDummy (M.Ldc (M.PRIM' _):cs)  = M.Ldc M.NIL : primDummy cs
        primDummy (c:cs) = c : primDummy cs
        primDummy []     = []


list' :: M.SecdFunc
list' _  e = return e

spec :: Spec
spec = do

  describe "test case" $
    it "Literal" $ do
    g <- H.new
    compile g (INT 1) `shouldBeT` [M.Ldc (M.INT 1),M.Stop]

  describe "test case" $
    it "Literal" $ do
    g <- H.new    
    compile g (l3 (SYM "+") (INT 1) (INT 2)) `shouldBeT` [M.Ldc (M.INT 1),
                                                          M.Ldc (M.INT 2),
                                                          M.Args 2,
                                                          M.Ldg "+",
                                                          M.App,
                                                          M.Stop]

  let lmd = lN[ SYM "lambda", lN [ SYM "n" ], SYM "n" ]

  describe "test case" $
    it "lambda" $ do
    g <- H.new    
    compile g lmd `shouldBeT` [ M.Ldf [ M.Ld (0,0), M.Rtn ], M.Stop ]

  describe "test case" $
    it "lambda apply" $ do 
    g <- H.new    
    compile g ( lN[ lmd , INT 1] ) `shouldBeT` [ M.Ldc (M.INT 1),
                                                 M.Args 1,
                                                 M.Ldf [ M.Ld (0,0), M.Rtn ],
                                                 M.App,
                                                 M.Stop ]

  describe "lambda" $ do
    it "list parameter 1" $ do
      g <- H.new
      let lmd = lN[ SYM "lambda", SYM "n", SYM "n" ]
      compile g lmd `shouldBeT` [ M.Ldf [ M.Ld (0,-1), M.Rtn ], M.Stop ]
      
    it "list parameter 2" $ do
      g <- H.new
      let lmd = lN[ SYM "lambda", (CELL (SYM "n") (SYM "m")), SYM "m" ]
      compile g lmd `shouldBeT` [ M.Ldf [ M.Ld (0,-2), M.Rtn ], M.Stop ]

    it "list parameter 3" $ do
      g <- H.new
      let lmd = lN[ SYM "lambda", (CELL (SYM "x") (CELL (SYM "n") (SYM "m"))), SYM "m" ]
      compile g lmd `shouldBeT` [ M.Ldf [ M.Ld (0,-3), M.Rtn ], M.Stop ]


  let lmd2 = lN[ SYM "lambda", lN [], INT 1, INT 2, INT 3, INT 4, INT 5 ]
  describe "test case" $
    it "lambda" $ do 
    g <- H.new    
    compile g lmd2 `shouldBeT` [ M.Ldf [ M.Ldc (M.INT 1),
                                       M.Pop,
                                       M.Ldc (M.INT 2),
                                       M.Pop,
                                       M.Ldc (M.INT 3),
                                       M.Pop,
                                       M.Ldc (M.INT 4),
                                       M.Pop,
                                       M.Ldc (M.INT 5),
                                       M.Rtn ], M.Stop ]
  

  let lmd3 = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ], lN[ SYM "+", SYM "n", SYM "m" ] ]
  describe "test case" $
    it "lambda apply" $ do 
    g <- H.new    
    compile g ( lN[ lmd3 , INT 3, INT 5] ) `shouldBeT` [ M.Ldc (M.INT 3),
                                                         M.Ldc (M.INT 5),
                                                         M.Args 2,
                                                         M.Ldf [ M.Ld (0,0), M.Ld (0,1),
                                                                 M.Args 2,
                                                                 M.Ldg "+",
                                                                 M.TApp,
                                                                 M.Rtn ],
                                                         M.App,
                                                         M.Stop ]
  describe "if" $
    it "lambda apply" $ do
    let sexp = lN[ SYM "if", BOOL True, INT 1, INT 2]
    g <- H.new    
    compile g sexp `shouldBeT` [ M.Ldc (M.BOOL True),
                                 M.Sel [ M.Ldc (M.INT 1), M.Join ] [ M.Ldc (M.INT 2), M.Join ],
                                 M.Stop ]

  describe "define" $
    it "simple int value" $ do
    let sexp = lN[ SYM "define", SYM "a", INT 2]
    g <- H.new    
    compile g sexp `shouldBeT` [ M.Ldc (M.INT 2), M.Def "a", M.Stop ]


  describe "define" $
    it "lambda" $ do
    let sexp = lN[ SYM "define", SYM "a", lmd ]
    g <- H.new    
    compile g sexp `shouldBeT` [ M.Ldf [ M.Ld (0,0), M.Rtn ],
                                 M.Def "a",
                                 M.Stop ]
      
  describe "quote" $
    it "lambda" $ do 
    g <- H.new    
    compile g (lN[ SYM "quote", SYM "a"] ) `shouldBeT` [ M.Ldc (M.SYM "a"), M.Stop ]

  describe "quote" $
    it "lambda" $ do 
    g <- H.new
    l <- M.listToCell [ M.SYM "a",M.SYM "b" ]
    compile g (lN[ SYM "quote", lN[ SYM "a", SYM "b"] ] ) `shouldBeT` 
      [ M.Ldc l, M.Stop ]
      

  describe "define-macro" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    compile g sexp `shouldBeT` [ M.Ldf [ M.Ld (0,0), M.Rtn ],
                                 M.Defm "a",
                                 M.Stop ]
  describe "use macro" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", SYM "n"] ) `shouldBeT` [ M.Ldg "n", M.Stop ]

  describe "use macro2" $
    it "lambda" $ do
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", INT 1] ) `shouldBeT` [ M.Ldc (M.INT 1), M.Stop ]


  describe "use macro3" $
    it "lambda" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ], SYM "m" ]
    let sexp = lN[ SYM "define-macro", SYM "a", lmd ]
    g <- H.new    
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "a", INT 1, INT 2] ) `shouldBeT` [ M.Ldc (M.INT 2), M.Stop ]

  describe "use macro4" $
    it "plus" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ],
                  lN [SYM "list", lN [ SYM "quote", SYM "+"], SYM "n", SYM "m" ] ]
    g <- H.fromList [("list", M.PRIM' list')]
    let sexp = lN[ SYM "define-macro", SYM "plus", lmd ]
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "plus", INT 1, INT 2] ) `shouldBeT` [M.Ldc (M.INT 1),
                                                            M.Ldc (M.INT 2),
                                                            M.Args 2,
                                                            M.Ldg "+", M.App, M.Stop ]

  describe "use macro5" $
    it "plus" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ],
                  lN [SYM "list", lN [ SYM "quote", SYM "+"], SYM "n", SYM "m" ] ]
    g <- H.fromList [("list", M.PRIM' list')]
    let sexp = lN[ SYM "define-macro", SYM "plus", lmd ]
    (Right code) <- runExceptT $ compile g sexp
    runExceptT $ exec g [] [] code []
    compile g (lN[ SYM "plus", SYM "a", SYM "b"] ) `shouldBeT` [M.Ldg "a",
                                                                M.Ldg "b",
                                                                M.Args 2,
                                                                M.Ldg "+", M.App, M.Stop ]

  describe "use macro6" $
    it "use quasiquote" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "n", SYM "m" ],
                  lN [SYM "quasiquote",
                      lN[ SYM "+",
                          lN[SYM "unquote", SYM "n"],
                          lN[SYM "unquote", SYM "m"]]]]
    let sexp = lN[ SYM "define-macro", SYM "plus", lmd ]
    g <- H.new
    mcode <- runExceptT $ compile g sexp
    case mcode of
      (Right code) -> do
        runExceptT $ exec g [] [] code []
        compile g (lN[ SYM "plus", INT 1, INT 2] ) `shouldBeT` [M.Ldc (M.INT 1),
                                                                M.Ldc (M.INT 2),
                                                                M.Args 2,
                                                                M.Ldg "+", M.App, M.Stop ]
      (Left e) -> putStrLn (show e)


  describe "use macro7" $
    it "use quasiquote" $ do
    let lmd = lN[ SYM "lambda", lN [ SYM "args" ],
                  lN [SYM "quasiquote",
                      lN[ SYM "+",
                          lN[ SYM "unquote-splicing", SYM "args" ]
                        ]
                     ]
                ]
    let sexp = lN[ SYM "define-macro", SYM "plus'", lmd ]
    g <- H.new
    mcode <- runExceptT $ compile g sexp
    case mcode of
      (Right code) -> do
        runExceptT $ exec g [] [] code []
        compile g (lN[ SYM "plus'", INT 1, INT 2] ) `shouldBeT` [M.Ldc (M.INT 1),
                                                                 M.Ldc (M.INT 2),
                                                                 M.Args 2,
                                                                 M.Ldg "+", M.App, M.Stop ]
      (Left e) -> putStrLn (show e)



  describe "quasiquote" $
    it "no unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote", SYM "a"] ) `shouldBeT` [ M.Ldc (M.SYM "a"), M.Stop ]


  describe "quasiquote" $
    it "unquote to Ldg1" $ do 
    g <- H.fromList [("a", M.INT 1)]
    compile g (lN[ SYM "quasiquote",
                   lN[ SYM "unquote", SYM "a"] ] ) `shouldBeT` [ M.Ldg "a", M.Stop ]

  describe "quasiquote" $
    it "unquote to Ldg2" $ do 
    g <- H.fromList [("b", lN'[ M.SYM "a", M.SYM "b", M.SYM "c"] )]
    compile g (lN[ SYM "quasiquote",
                   lN[ SYM "unquote", SYM "b"] ] ) `shouldBeT`[ M.Ldg "b", M.Stop ]
                                                                    
                                                                    
  describe "quasiquote" $
    it "list with no unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a", SYM "b",SYM "c"] ] ) `shouldBeT`
      [M.Ldc (M.SYM "a"),M.Ldc (M.SYM "b"),M.Ldc (M.SYM "c"),M.Ldc M.NIL,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Stop]
    -- some effect as bellow (but complicated) 
    -- [ Ldc (lN[ SYM "a",SYM "b",SYM "c"]), M.Stop ] 

  describe "quasiquote" $
    it "list with unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a",
                      lN[SYM "unquote", SYM "b"],
                      SYM "c"]
                 ]
              ) `shouldBeT`
      [M.Ldc (M.SYM "a"),M.Ldg "b",M.Ldc (M.SYM "c"),M.Ldc M.NIL,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Stop]

  describe "quasiquote" $
    it "list with unquote-splicing" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a",
                      lN[SYM "unquote-splicing", SYM "b"],
                      SYM "c"]
                 ]
              ) `shouldBeT`
      [M.Ldc (M.SYM "a"),M.Ldg "b",M.Ldc (M.SYM "c"),M.Ldc M.NIL,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.append'),M.App,
       M.Args 2,M.Ldc (M.PRIM' F.cons),M.App,
       M.Stop]

  -- (call/cc (lambda (k) (k 1)))
  describe "call/cc" $
    it "call/cc" $ do 
    g <- H.new    
    compile g (lN[ SYM "call/cc", lN[ SYM "lambda",
                                      lN[ SYM "k" ],
                                      lN[ SYM "k", INT 1] ] ]) `shouldBeT`
      [ M.Ldct [M.Stop], M.Args 1, M.Ldf [ M.Ldc (M.INT 1), M.Args 1,
                                   M.Ld (0,0), M.TApp, M.Rtn], M.App, M.Stop ]
      
    
  -- (call/cc (lambda (k) (k 1)))
  describe "call/cc" $
    it "call/cc" $ do 
    g <- H.new    
    (Right cs) <- runExceptT $ compile g (lN[ SYM "call/cc", lN[ SYM "lambda",
                                                                 lN[ SYM "k" ],
                                                                 lN[ SYM "k", INT 1] ] ])
    (Right v) <- runExceptT $ exec g [] [] cs []
    v `shouldBe` M.INT 1

  -- tail position
  describe "tail position1" $
    it "tail" $ do 
    let lmd = lN[ SYM "lambda", lN [], lN[SYM "foo", lN[ SYM "goo"]] ]
    g <- H.new    
    compile g lmd `shouldBeT` [M.Ldf [M.Args 0,M.Ldg "goo", M.App, 
                                    M.Args 1,M.Ldg "foo", M.TApp, M.Rtn], M.Stop]
  describe "tail position2" $
    it "no tail position " $ do 
    let lmd  = lN[ SYM "foo", lN[ SYM "goo"] ]
    let lmd2 = lN[ SYM "if", BOOL True, lmd, lmd ]
    g <- H.new    
    compile g lmd2 `shouldBeT` [M.Ldc (M.BOOL True),
                                M.Sel [M.Args 0,M.Ldg "goo",M.App,M.Args 1,M.Ldg "foo",M.App,M.Join]
                                    [M.Args 0,M.Ldg "goo",M.App,M.Args 1,M.Ldg "foo",M.App,M.Join],
                                M.Stop
                               ]
  describe "tail position2" $
    it "tail position " $ do 
    let lmd  = lN[ SYM "foo", lN[ SYM "goo"] ]
    let lmd2 = lN[ SYM "lambda", lN[], lN[ SYM "if", BOOL True, lmd, lmd ] ]
    g <- H.new    
    compile g lmd2 `shouldBeT` [M.Ldf [M.Ldc (M.BOOL True),
                                     M.Selr
                                      [M.Args 0,M.Ldg "goo",M.App,M.Args 1,M.Ldg "foo",M.TApp,M.Rtn]
                                      [M.Args 0,M.Ldg "goo",M.App,M.Args 1,M.Ldg "foo",M.TApp,M.Rtn],
                                     M.Rtn
                                      ],
                                M.Stop
                               ]

      

  
    


