module NNet where
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as ByteMe

import Control.Monad
import Data.Ord
import Data.Int
import Data.List
import System.Random

type Brain =  IO [([Float], [[Float]])]
-- Makes a new brain with the array of ints as the
--
{-@newBrain :: {v:[Int]| length' v>0} -> Brain@-}
newBrain :: [Int] -> Brain -- IO [([Float], [[Float]])]


width = 28
height = 28
-- Box–Muller transform
bmt :: Float -> IO Float

-- This is to move from layer to layer
pushThroughLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
feed :: [Float] -> [([Float], [[Float]])] -> [Float]
reverseBrain :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
δs :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
processNN :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
sigmoidSimple :: Float -> Float
num2Str :: Integral a => a -> Char
cost :: (Ord t, Num t) => t -> t -> t
descend :: Fractional c => [c] -> [c] -> [c]

bmt scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

newBrain (hs:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ bmt 0.01) (hs:ts) ts
newBrain _ = error "New Brain: Empty List provided"

sigmoidSimple x | x < 0      = 0
        | otherwise  = 1

pushThroughLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed = foldl' ((((max 0) <$>) . ) . pushThroughLayer)

reverseBrain xs = foldl' (\(av:avt, zs) (bs, wms) -> (((max 0) <$> (pushThroughLayer av (bs, wms))):(av:avt), (pushThroughLayer av (bs, wms)):zs)) ([xs], [])


{-@cost :: Num a=> a -> a -> {v:a| v <= 0}@-}
cost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

δs vec1 vec2 layers = (reverse (avh:avt), f (transpose . snd <$> reverse layers) zvs [initalDel])
  where
    f _ [] dvs = dvs
    f (wm:wms) (zvh:zvt) (dvh:dvt) = f wms zvt $ (:(dvh:dvt)) $ zipWith (*) [sum $ zipWith (*) row dvh | row <- wm] (sigmoidSimple <$> zvh)
    f _ _ _  = error "undefined case in δs"
    (avh:avt, zv:zvs) = reverseBrain vec1 layers
    initalDel = zipWith (*) (zipWith cost avh vec2) (sigmoidSimple <$> zv)


descend aVecs delVecs = zipWith (-) aVecs ((0.002 *) <$> delVecs)

processNN vec1 vec2 layers = zip (zipWith descend (fst <$> layers) dvs) $ zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
      (snd <$> layers) avs dvs
  where
    (avs, dvs) = δs vec1 vec2 layers

getimg s n = fromIntegral . ByteMe.index s . (n*width^2 + 16 +) <$> [0..height^2 - 1]
getx     s n = (/ 256) <$> getimg s n
getlabel s n = fromIntegral $ ByteMe.index s (n + 8)
gety    s n = fromIntegral . fromEnum . (getlabel s n ==) <$> [0..9]


num2Str n =  let i = fromIntegral n * 2 `div` 256 in head (show i)

train :: String -> String -> Brain -- IO [([Float], [[Float]])]
train imgF lblF = do
  b <- newBrain [784, 30, 10]
  trainImg <- ((decompress <$> ) . ByteMe.readFile) imgF
  trainLbl <- ((decompress <$> ) . ByteMe.readFile) lblF
  let
    smart = (foldl' (\b n -> processNN (getx trainImg n) (gety trainLbl n) b)) b [   0.. 9999]
  return smart


testNN :: String -> String -> Brain {-IO [([Float], [[Float]])]-} -> IO Int
testNN imgF lblF brain = do
  smart <- brain
  testimg <- ((decompress <$> ) . ByteMe.readFile) imgF
  testlbls <- ((decompress <$> ) . ByteMe.readFile) lblF
  let
    bestOf = fst . maximumBy (comparing snd) . zip [0..]
    answers = getlabel testlbls <$> [0..9999]
    gusses = bestOf . (\n -> feed (getx testimg n) smart) <$> [0..9999]
  return $ sum (fromEnum <$> zipWith (==) gusses answers) `div` 100

writeNNToFile :: String -> Brain -> Brain
writeNNToFile fName brain= do
  smart <- brain
  writeFile fName $ show smart
  return smart

readNNFile :: String -> Brain
readNNFile fName = do
  sSmart <- readFile fName
  let smart = read sSmart :: [([Float], [[Float]])]
  return smart
