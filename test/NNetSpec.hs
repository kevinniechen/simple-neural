module Main where
import NNet
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import System.IO.Unsafe
--import Test.Framework.Providers.QuickCheck2 (testProperty)

ftest_imageF = "data/t10k-images-idx3-ubyte.gz"
ftest_labelF = "data/t10k-labels-idx1-ubyte.gz"
train_imageF = "data/train-images-idx3-ubyte.gz"
train_labelF = "data/train-labels-idx1-ubyte.gz"

fnnF = readNNFile "data/eg-trained-model.txt"

test_NN = (testNN ftest_imageF ftest_labelF $ train train_imageF train_labelF) 
--main = htfMain htf_thisModulesTests
{-main = do
    putStrLn "This test always fails!"
    exitFailure-}

prop_relu num = relu num >= 0
main = 
  do
    val <- test_NN
    defaultMain $
      testGroup "Tests"
        [ testGroup "Checking Helpers in NNet"
            [testProperty "relu" prop_relu              
            ]
        

        , testGroup "Checking if the NNet is working well enough"
            [  {-let val = (unsafePerformIO test_NN) in -}
                  testCase ("Accuracy test: " ++ show val ++ "%") $
                  val `compare` 74 @?= GT 

            ]
        ]
