

module CompilerSpec where

import Compiler
import Secd
import SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified Data.HashTable.IO as H
import qualified SecdFuncs as F

-- helpers
l2 :: SExpr -> SExpr -> SExpr
l2 x y = (CELL x (CELL y NIL))

l3 :: SExpr -> SExpr -> SExpr -> SExpr
l3 x y z = (CELL x (CELL y (CELL z NIL)))

lN :: [SExpr] -> SExpr
lN (x:xs) = (CELL x (lN xs))
lN []     = NIL


shouldBeT ma b = do
  xa <- runExceptT ma
  case xa of
    Right a -> a `compareCode` b
    Left  e ->  putStrLn (show e)

-- round about for failure of PRIM' equality
compareCode a b =
  primDummy a `shouldBe` primDummy b
  where primDummy (Ldc (PRIM' _):cs)  = Ldc NIL : primDummy cs
        primDummy (c:cs) = c : primDummy cs
        primDummy []     = []


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
                                                               TApp,
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
        compile g (lN[ SYM "plus", INT 1, INT 2] ) `shouldBeT` [Ldc (INT 1),
                                                                Ldc (INT 2),
                                                                Args 2,
                                                                Ldg "+", App, Stop ]
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
        compile g (lN[ SYM "plus'", INT 1, INT 2] ) `shouldBeT` [Ldc (INT 1),
                                                                 Ldc (INT 2),
                                                                 Args 2,
                                                                 Ldg "+", App, Stop ]
      (Left e) -> putStrLn (show e)



  describe "quasiquote" $
    it "no unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote", SYM "a"] ) `shouldBeT` [ Ldc (SYM "a"), Stop ]


  describe "quasiquote" $
    it "unquote to Ldg1" $ do 
    g <- H.fromList [("a", INT 1)]
    compile g (lN[ SYM "quasiquote",
                   lN[ SYM "unquote", SYM "a"] ] ) `shouldBeT` [ Ldg "a", Stop ]

  describe "quasiquote" $
    it "unquote to Ldg2" $ do 
    g <- H.fromList [("b", lN[ SYM "a", SYM "b", SYM "c"] )]
    compile g (lN[ SYM "quasiquote",
                   lN[ SYM "unquote", SYM "b"] ] ) `shouldBeT`[ Ldg "b", Stop ]
                                                                    
                                                                    
  describe "quasiquote" $
    it "list with no unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a", SYM "b",SYM "c"] ] ) `shouldBeT`
      [Ldc (SYM "a"),Ldc (SYM "b"),Ldc (SYM "c"),Ldc NIL,
       Args 2,Ldc (PRIM' F.cons),App,
       Args 2,Ldc (PRIM' F.cons),App,
       Args 2,Ldc (PRIM' F.cons),App,
       Stop]
    -- some effect as bellow (but complicated) 
    -- [ Ldc (lN[ SYM "a",SYM "b",SYM "c"]), Stop ] 

  describe "quasiquote" $
    it "list with unquote" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a",
                      lN[SYM "unquote", SYM "b"],
                      SYM "c"]
                 ]
              ) `shouldBeT`
      [Ldc (SYM "a"),Ldg "b",Ldc (SYM "c"),Ldc NIL,
       Args 2,Ldc (PRIM' F.cons),App,
       Args 2,Ldc (PRIM' F.cons),App,
       Args 2,Ldc (PRIM' F.cons),App,
       Stop]

  describe "quasiquote" $
    it "list with unquote-splicing" $ do 
    g <- H.new    
    compile g (lN[ SYM "quasiquote",
                   lN[SYM "a",
                      lN[SYM "unquote-splicing", SYM "b"],
                      SYM "c"]
                 ]
              ) `shouldBeT`
      [Ldc (SYM "a"),Ldg "b",Ldc (SYM "c"),Ldc NIL,
       Args 2,Ldc (PRIM' F.cons),App,
       Args 2,Ldc (PRIM' F.append'),App,
       Args 2,Ldc (PRIM' F.cons),App,
       Stop]

  -- (call/cc (lambda (k) (k 1)))
  describe "call/cc" $
    it "call/cc" $ do 
    g <- H.new    
    compile g (lN[ SYM "call/cc", lN[ SYM "lambda",
                                      lN[ SYM "k" ],
                                      lN[ SYM "k", INT 1] ] ]) `shouldBeT`
      [ Ldct [Stop], Args 1, Ldf [ Ldc (INT 1), Args 1,
                                   Ld (0,0), TApp, Rtn], App, Stop ]
      
    
  -- (call/cc (lambda (k) (k 1)))
  describe "call/cc" $
    it "call/cc" $ do 
    g <- H.new    
    (Right cs) <- runExceptT $ compile g (lN[ SYM "call/cc", lN[ SYM "lambda",
                                                                 lN[ SYM "k" ],
                                                                 lN[ SYM "k", INT 1] ] ])
    (Right v) <- runExceptT $ exec g [] [] cs []
    v `shouldBe` INT 1

  -- tail position
  describe "tail position1" $
    it "tail" $ do 
    let lmd = lN[ SYM "lambda", lN [], lN[SYM "foo", lN[ SYM "goo"]] ]
    g <- H.new    
    compile g lmd `shouldBeT` [Ldf [Args 0,Ldg "goo", App, 
                                    Args 1,Ldg "foo", TApp, Rtn], Stop]
  describe "tail position2" $
    it "no tail position " $ do 
    let lmd  = lN[ SYM "foo", lN[ SYM "goo"] ]
    let lmd2 = lN[ SYM "if", BOOL True, lmd, lmd ]
    g <- H.new    
    compile g lmd2 `shouldBeT` [Ldc (BOOL True),
                                Sel [Args 0,Ldg "goo",App,Args 1,Ldg "foo",App,Join]
                                    [Args 0,Ldg "goo",App,Args 1,Ldg "foo",App,Join],
                                Stop
                               ]
  describe "tail position2" $
    it "tail position " $ do 
    let lmd  = lN[ SYM "foo", lN[ SYM "goo"] ]
    let lmd2 = lN[ SYM "lambda", lN[], lN[ SYM "if", BOOL True, lmd, lmd ] ]
    g <- H.new    
    compile g lmd2 `shouldBeT` [Ldf [Ldc (BOOL True),
                                     Selr
                                      [Args 0,Ldg "goo",App,Args 1,Ldg "foo",TApp,Rtn]
                                      [Args 0,Ldg "goo",App,Args 1,Ldg "foo",TApp,Rtn],
                                     Rtn
                                      ],
                                Stop
                               ]

      

  
    


