import Codec.Compression.GZip (decompress)
import NNet
import qualified Data.ByteString.Lazy as ByteMe

import Debug.Trace
import Control.Monad
import Data.Functor
import Data.Ord
import Data.List
import System.Random

main = do
  timgs <- ((decompress  <$>) . ByteMe.readFile) "../data/train-images-idx3-ubyte.gz"
  tlbls <- ((decompress  <$>) . ByteMe.readFile) "../data/train-labels-idx1-ubyte.gz"
  testimgs  <- ((decompress  <$>) . ByteMe.readFile) "../data/t10k-images-idx3-ubyte.gz"
  testlbls  <- ((decompress  <$>) . ByteMe.readFile) "../data/t10k-labels-idx1-ubyte.gz"
  b <- newBrain [784, 30, 10]
  n <- (`mod` 10000) <$> randomIO
  putStr . unlines $
    take 28 $ take 28 <$> iterate (drop 28) (num2Str <$> getimg testimgs n)

  let
    example = getx testimgs n
    batch1 = foldl' (\b n -> processNN (getx timgs n) (gety tlbls n) b) b [0 ..5000]
    batch2 = foldl' (\b n -> processNN (getx timgs n) (gety tlbls n) b) batch1 [5001 ..6999]
    batch3 = foldl' (\b n -> processNN (getx timgs n) (gety tlbls n) b) batch2 [7000 ..7999]
    batch4 = foldl' (\b n -> processNN (getx timgs n) (gety tlbls n) b) batch3 [8000 ..9999]
    smart = batch4
    cute d score = "->(" ++ show d ++ ")\t" ++ show (round $ 100 * min 1 score)
    bestOf = fst . maximumBy (comparing snd) . zip [0..]

  forM_ [b, batch1, batch2, batch3, batch4] $ putStrLn . unlines . zipWith cute [0..9] . feed example

  putStrLn $ "Is it: " ++ show (bestOf $ feed example smart) ++ "?"

  let guesses = bestOf . (\n -> feed (getx testimgs n) smart) <$> [0..9999]
  let answers = getlabel testlbls <$> [0..9999]
  putStrLn $ show (sum $ fromEnum <$> zipWith (==) guesses answers) ++
    " / 10000"
