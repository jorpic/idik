{-# LANGUAGE QuasiQuotes #-}
import Test.Hspec
import Data.Text (Text)
import NeatInterpolation (text)

import K


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    idiom "575. Kronecker delta of x and y"
        [text|
            x:0 0 1 1
            y:0 1 0 1
            x=y
        |]

        (Let
            "x" (Arr [I 0, I 0, I 1, I 1])
            $ Let
                "y" (Arr [I 0, I 1, I 0, I 1])
                    $ Op2 Eq (Var "x") (Var "y")
        )

        (Arr [I 1, I 0, I 0, I 1])

idiom :: String -> Text -> K.Expr -> K.Expr -> SpecWith ()
idiom name prog ast result
    = describe name $ do
        -- specify "parse" $ K.parse prog `shouldBe` Right ast
        specify "eval" $ K.eval ast `shouldBe` Right result
