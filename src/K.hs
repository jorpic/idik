module K
    ( parse
    , eval
    , Op2(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import Data.Map qualified as Map

data Op2 = Eq
    deriving (Eq, Show)

data Expr
    = I Integer
    | Arr [Expr]
    | Let String Expr Expr
    | Op2 Op2 Expr Expr
    | Var String
    deriving (Eq, Show)


parse :: Text -> Either String Expr
parse _ = Left "Not implemented"

eval :: Expr -> Either String Expr
eval = eval' Map.empty
    where
        eval' :: Map.Map String Expr -> Expr -> Either String Expr
        eval' vars = \case
            i@(I _) -> pure i

            Arr xs -> Arr <$> traverse ev xs

            Let var val expr ->
                let vars' = Map.insert var val vars
                 in eval' vars' expr

            -- The = verb compares operands for equality, and when applied to
            -- two vectors it performs an element-wise comparison.
            Op2 Eq x y -> do
                x' <- ev x
                y' <- ev y
                case (x', y') of
                    (Arr a, Arr b) -> Arr <$> sequence (zipWith eval_Eq a b)
                    _ -> eval_Eq x' y'

            Var v -> case Map.lookup v vars of
                Just x -> ev x
                Nothing -> Left $ "Variable " ++ v ++ " is not defined"

            where
                ev = eval' vars

                eval_Eq :: Expr -> Expr -> Either String Expr
                eval_Eq x y = do
                    x' <- ev x
                    y' <- ev y
                    pure $ I (if x' == y' then 1 else 0)
