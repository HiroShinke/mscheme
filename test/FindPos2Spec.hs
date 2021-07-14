

module FindPos2Spec where

import Data.IORef
import Mutable.Compiler
import Mutable.Secd
import Mutable.SExpr
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified Data.HashTable.IO as H

lN :: [SExpr] -> IO SExpr
lN (x:xs) = CELL <$> newIORef x <*> (lN xs >>= newIORef)
lN []     = return NIL

frame :: [SExpr] -> IO [IORef SExpr]
frame = mapM newIORef


shouldBeIO ma mb = do
  a <- ma
  b <- mb
  a `shouldBe` b

spec :: Spec
spec = do
  describe "findPos" $ do
    it "(0,0)" $ do
      x <- lN[ SYM "n", SYM "m" ]
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "n" fs `shouldBeIO` return (Just (0,0))

    it "(0,1)" $ do
      x <- lN[ SYM "n", SYM "m" ]
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "m" fs `shouldBeIO` return (Just (0,1))

    it "(1,1)" $ do
      x <- lN[ SYM "n", SYM "m" ]
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "b" fs `shouldBeIO` return (Just (1,1))

  describe "findPos" $ do
    it "(0,-1)" $ do
      x <- return $ SYM "n"
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "n" fs `shouldBeIO` return (Just (0,-1))

    it "(0,-2)" $ do
      x <- CELL <$> newIORef (SYM "n") <*> newIORef (SYM "m")
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "m" fs `shouldBeIO` return (Just (0,-2))

    it "Nothing" $ do
      x <- CELL <$> newIORef (SYM "n") <*> newIORef (SYM "m")
      y <- lN[ SYM "a", SYM "b" ]
      fs <- frame [x,y]
      findPos "z" fs `shouldBeIO` return Nothing



