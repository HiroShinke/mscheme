
module SExpr2Spec where

import Mutable.SExpr
import Data.IORef
import qualified System.IO.Silently as Silently
import Test.Hspec (Spec, describe, it, shouldBe)
import Control.Monad.Trans.Except
import qualified SecdFuncs as F
import qualified Data.HashTable.IO as H
type HashTable k v = H.CuckooHashTable k v

shouldBeM x y = do
  x' <- x
  y' <- y
  b <- equalExpr x' y'
  b `shouldBe` True 

spec :: Spec
spec = do
  describe "basics" $
    it "constructer" $ do
    cons (INT 1) (INT 2) `shouldBeM` cons (INT 1) (INT 2)

  describe "basics" $
    it "constructer" $ do
    cons (INT 1) (INT 2) `shouldBeM` cons (INT 1) (INT 2)
      
      
    
