module K
    ( parse
    , eval
    , Op2(..)
    , Expr(..)
    ) where

import Data.Text (Text)

data Op2 = Eq | Gt
    deriving (Eq, Show)

data Expr
    = I Integer
    | Arr [Expr]
    | Var String
    | Let String Expr Expr
    | Op2 Op2 Expr Expr
    deriving (Eq, Show)


parse :: Text -> Either String Expr
parse _ = Left "Not implemented"

eval :: Expr -> Either String Expr
eval _ = Left "Not implemented"
