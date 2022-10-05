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
            "x" (Vec [I 0, I 0, I 1, I 1])
            $ Let
                "y" (Vec [I 0, I 1, I 0, I 1])
                    $ Op2 Eq (Var "x") (Var "y")
        )

        (Vec [I 1, I 0, I 0, I 1])

    idiom "571. x but not y"
        [text|
            x:0 1 0 1
            y:0 0 1 1
            x>y
        |]

        (Let
            "x" (Vec [I 0, I 1, I 0, I 1])
            $ Let
                "y" (Vec [I 0, I 0, I 1, I 1])
                    $ Op2 Gt (Var "x") (Var "y")
        )

        (Vec [I 0, I 1, I 0, I 0])

    idiom "570. x implies y"
        [text|
            x:0 1 0 1
            y:0 0 1 1
            ~x>y
        |]

        (Let
            "x" (Vec [I 0, I 1, I 0, I 1])
            $ Let
                "y" (Vec [I 0, I 0, I 1, I 1])
                    $ Op1 Neg
                        $ Op2 Gt (Var "x") (Var "y")
        )

        (Vec [I 1, I 0, I 1, I 1])

    idiom "573. exclusive or"
        [text|
            x:0 0 1 1
            y:0 1 0 1
            ~x=y
        |]

        (Let
            "x" (Vec [I 0, I 0, I 1, I 1])
            $ Let
                "y" (Vec [I 0, I 1, I 0, I 1])
                    $ Op1 Neg
                        $ Op2 Eq (Var "x") (Var "y")
        )

        (Vec [I 0, I 1, I 1, I 0])

    idiom "41.0 indices of ones in Boolean vector x"
        [text|
            x:0 0 1 0 1 0 0 0 1 0
            &x
        |]

        (Let
            "x" (Vec $ map I [0, 0, 1, 0, 1, 0, 0, 0, 1, 0])
            $ Op1 Where (Var "x")
        )

        (Vec [I 2, I 4, I 8])

    idiom "41.1 generating vector of zeros"
        "&7"
        (Op1 Where (I 7))
        (iVec $ replicate 7 0)

    idiom "41.2 generating vector of ones"
        "~&6"
        (Op1 Neg $ Op1 Where (I 6))
        (iVec $ replicate 6 1)

idiom :: String -> Text -> K.Expr -> K.Expr -> SpecWith ()
idiom name prog ast result
    = describe name $ do
        -- specify "parse" $ K.parse prog `shouldBe` Right ast
        specify "eval" $ K.eval ast `shouldBe` Right result
