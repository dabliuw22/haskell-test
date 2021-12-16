import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Example Test" $ do
    it "Should be successful" $ do
      True `shouldBe` True
