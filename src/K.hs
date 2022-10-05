module K
    ( parse
    , eval
    , Op2(..)
    , Op1(..)
    , Expr(..)
    , iVec
    ) where

import Data.Text (Text)
import Data.Map qualified as Map

data Op1 = Neg | Where
    deriving (Eq, Ord, Show)

data Op2 = Eq | Gt
    deriving (Eq, Ord, Show)

data Expr
    = I Integer
    | Vec [Expr]
    | Let String Expr Expr
    | Op1 Op1 Expr
    | Op2 Op2 Expr Expr
    | Var String
    deriving (Eq, Ord, Show)

iVec :: [Integer] -> Expr
iVec = Vec . map I

parse :: Text -> Either String Expr
parse _ = Left "Not implemented"

eval :: Expr -> Either String Expr
eval = eval' Map.empty
    where
        eval' :: Map.Map String Expr -> Expr -> Either String Expr
        eval' vars = \case
            i@(I _) -> pure i

            Vec xs -> Vec <$> traverse ev xs

            Let var val expr ->
                let vars' = Map.insert var val vars
                 in eval' vars' expr

            -- Some operators
            Op1 Where x
                -> ev x >>= \case
                    Vec xs -> pure
                        $ Vec $ concat
                            [ replicate (fromInteger y) (I i)
                            | (I y,i) <- zip xs [0..]
                            ]
                    I i -> pure
                        $ Vec $ replicate (fromInteger i) (I 0)
                    _ -> Left $ "Invalid argument type for Where"
            Op1 op x
                -> ev x >>= \case
                    Vec x' -> Vec <$> traverse (evalOp1 op) x'
                    x' -> evalOp1 op x'

            Op2 op x y -> do
                x' <- ev x -- FIXME: eval to normal form
                y' <- ev y
                case (x', y') of
                    -- Perform operation element-wise if it is applied to
                    -- vectors.
                    (Vec a, Vec b)
                        -> Vec <$> sequence (zipWith (evalOp2 op) a b)
                    _ -> evalOp2 op x' y'

            Var v -> case Map.lookup v vars of
                Just x -> ev x
                Nothing -> Left $ "Variable " ++ v ++ " is not defined"

            where
                ev = eval' vars

evalOp1 :: Op1 -> Expr -> Either String Expr
evalOp1 op x
    = pure $ case op of
        Neg -> if x > I 0 then I 0 else I 1

evalOp2 :: Op2 -> Expr -> Expr -> Either String Expr
evalOp2 op x y
    = pure $ case op of
        Eq -> if x == y then I 1 else I 0
        Gt -> if x >  y then I 1 else I 0
