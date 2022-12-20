import Test.Hspec

import Language.Bruh.AST
    
main :: IO ()
main = hspec $ do
  it "can parse expressions" $ do
    2 + 2 `shouldBe` EBinary BAdd (EInteger 2) (EInteger 2)
  it "can parse statements" $ do
    let x = VVar "x"
    x := 0 `shouldBe` SAssign x (EInteger 0)
