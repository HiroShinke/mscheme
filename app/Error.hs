

module Error where

class Error a where
    noMsg :: a
    strMsg :: String -> a

errNUM  = "Illegal argument, Number required"
errINT  = "Illegal argument, Integer required"
errNEA  = "Not enough arguments"
errCELL = "Illegal argument, List required"
errZERO = "Divide by zero"

