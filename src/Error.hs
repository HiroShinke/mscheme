

module Error where

import Control.Monad.Trans.Except

class Error a where
    noMsg :: a
    strMsg :: String -> a

errNUM  = "Illegal argument, Number required"
errINT  = "Illegal argument, Integer required"
errNEA  = "Not enough arguments"
errCELL = "Illegal argument, List required"
errZERO = "Divide by zero"

-- Scmエラーの定義
data ScmError = ScmError String String deriving (Show, Eq)

instance Error ScmError where
  noMsg    = ScmError "" ""
  strMsg s = ScmError  "" s

type Scm a    = ExceptT ScmError IO a

