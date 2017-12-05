module NNet.BasicSpec (spec) where
  
import Imports

spec :: Spec
spec = sigmoidSpec

sigmoidSpec :: Spec
sigmoidSpec = describe "Sigmoid function" $ do

  it "returns a positive number when given any input" $ property $
    \x -> sigmoidSimple x >= 0

  it "returns zero when given any negative input" $ property $
    \x -> x < 0 ==> sigmoidSimple x == 0

  it "returns one when given any positive input" $ property $
    \x -> x >= 0 ==> sigmoidSimple x == 1
