import Test.Hspec

import Language.Bruh.AST
    
main :: IO ()
main = hspec $ do
  it "can parse expressions" $ do
    2 + 2 `shouldBe` EBinary BAdd (EInteger 2) (EInteger 2)
