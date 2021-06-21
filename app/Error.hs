

module Error where

class Error a where
    noMsg :: a
    strMsg :: String -> a

