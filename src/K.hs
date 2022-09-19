module K
    ( parse
    , eval
    , Op2(..)
    , Expr(..)
    ) where

import Data.Text (Text)
import Data.Map qualified as Map

data Op2 = Eq | Gt
    deriving (Eq, Ord, Show)

data Expr
    = I Integer
    | Arr [Expr]
    | Let String Expr Expr
    | Op2 Op2 Expr Expr
    | Var String
    deriving (Eq, Ord, Show)


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

            Op2 op x y -> do
                x' <- ev x
                y' <- ev y
                case (x', y') of
                    -- Perform operation element-wise if it is applied to
                    -- vectors.
                    (Arr a, Arr b)
                        -> Arr <$> sequence (zipWith (evalOp op) a b)
                    _ -> evalOp op x' y'

            Var v -> case Map.lookup v vars of
                Just x -> ev x
                Nothing -> Left $ "Variable " ++ v ++ " is not defined"

            where
                ev = eval' vars

evalOp :: Op2 -> Expr -> Expr -> Either String Expr
evalOp op x y
    = pure $ case op of
        Eq -> if x == y then I 1 else I 0
        Gt -> if x >  y then I 1 else I 0
