module NNet where
import Codec.Compression.GZip (decompress)
import qualified Data.ByteString.Lazy as BS

import System.IO
import Control.Monad
import Data.Functor
import Data.Ord
import Data.List
import System.Random

type Brain =  IO [([Float], [[Float]])]
-- Makes a new brain with the array of ints as the
--
{-@newBrain :: {v:[Int]| length' v>0} -> Brain@-}
newBrain :: [Int] -> Brain -- IO [([Float], [[Float]])]

-- Box–Muller transform
gauss :: Float -> IO Float

-- This is to move from layer to layer
zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
feed :: [Float] -> [([Float], [[Float]])] -> [Float]
revaz :: [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
deltas :: [Float] -> [Float] -> [([Float], [[Float]])] -> ([[Float]], [[Float]])
learn :: [Float] -> [Float] -> [([Float], [[Float]])] -> [([Float], [[Float]])]
relu :: Float -> Float
render :: Integral a => a -> Char

gauss scale = do
  x1 <- randomIO
  x2 <- randomIO
  return $ scale * sqrt (-2 * log x1) * cos (2 * pi * x2)

newBrain (hs:ts) = zip (flip replicate 1 <$> ts) <$>
  zipWithM (\m n -> replicateM n $ replicateM m $ gauss 0.01) (hs:ts) ts

relu = max 0
relu' x | x < 0      = 0
        | otherwise  = 1

zLayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs

feed = foldl' (((relu <$>) . ) . zLayer)

revaz xs = foldl' (\(av:avt, zs) (bs, wms) -> let
  zs' = zLayer av (bs, wms) in ((relu <$> zs'):(av:avt), zs':zs)) ([xs], [])

{-@dCost :: Num a=> a -> a -> {v:a| v <= 0}@-}
dCost a y | y == 1 && a >= y = 0
          | otherwise        = a - y

deltas xv yv layers = let
  (avs@(av:_), zv:zvs) = revaz xv layers
  delta0 = zipWith (*) (zipWith dCost av yv) (relu' <$> zv)
  in (reverse avs, f (transpose . snd <$> reverse layers) zvs [delta0]) where
    f _ [] dvs = dvs
    f (wm:wms) (zv:zvs) dvs@(dv:_) = f wms zvs $ (:dvs) $
      zipWith (*) [sum $ zipWith (*) row dv | row <- wm] (relu' <$> zv)

eta = 0.002

descend av dv = zipWith (-) av ((eta *) <$> dv)

learn xv yv layers = let (avs, dvs) = deltas xv yv layers
  in zip (zipWith descend (fst <$> layers) dvs) $
    zipWith3 (\wvs av dv -> zipWith (\wv d -> descend wv ((d*) <$> av)) wvs dv)
      (snd <$> layers) avs dvs

getImage s n = fromIntegral . BS.index s . (n*28^2 + 16 +) <$> [0..28^2 - 1]
getX     s n = (/ 256) <$> getImage s n
getLabel s n = fromIntegral $ BS.index s (n + 8)
getY     s n = fromIntegral . fromEnum . (getLabel s n ==) <$> [0..9]


render n = let s = " .:oO@" in s !! (fromIntegral n * length s `div` 256)

train :: String -> String -> Brain -- IO [([Float], [[Float]])]
train imgF lblF = do
  b <- newBrain [784, 30, 10]
  [trainI, trainL] <- mapM ((decompress <$> ) . BS.readFile) [imgF, lblF]
  let
    bs = scanl (foldl' (\b n -> learn (getX trainI n) (getY trainL n) b)) b [
     [   0.. 999],
     [1000..2999],
     [3000..5999],
     [6000..9999]]
    smart = last bs
  return smart

 
