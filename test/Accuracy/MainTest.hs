module Main where
import NNet
import Test.Tasty
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

main = 
  do
    val <- test_NN
    defaultMain $
      testGroup "Tests"
        [ testGroup "Checking if the NNet is working well enough"
            [ 
                  testCase ("Accuracy test: " ++ show val ++ "%") $
                  val `compare` 74 @?= GT 

            ]
        ]
