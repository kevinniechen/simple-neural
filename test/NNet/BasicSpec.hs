module NNet.BasicSpec (spec) where
  
import Imports

spec :: Spec
spec = sigmoidSpec

sigmoidSpec :: Spec
sigmoidSpec = describe "Sigmoid" $ do

  it "returns a positive number when given any input" $ property $
    \x -> sigmoidSimple x >= 0

  it "returns zero when given any negative input" $ property $
    \x -> x < 0 ==> sigmoidSimple x == 0

  it "returns one when given any positive input" $ property $
    \x -> x >= 0 ==> sigmoidSimple x == 1

  

-- randomNumberGeneratorSpec :: Spec
-- randomNumberGeneratorSpec = describe "BMT" $ do

--   getIt :: IO f => f
--   getIt = do
--     a <- bmt 10
--     return (a <= 10)

--   it "returns a random number from 0 - input when given any input" $ property $
--     runIdentityT getIt `shouldReturn` (true)
