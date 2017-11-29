module NNet where
    import Codec.Compression.GZip (decompress)
    import qualified Data.ByteString.Lazy as BS
    import Data.Functor
    import System.Random
    
    gauss :: Float -> IO Float
    gauss stdev = do
      x1 <- randomIO
      x2 <- randomIO
      return $ stdev * sqrt(-2 * log x1) * cos (2 * pi * x2)
    
    relu :: Float -> Float
    relu = max 0
    relu' x | x < 0 = 0
            | otherwise = 1
    
    zLayer :: [Float] -> ([Float], [[Float]]) -> [Float]
    zlayer as (bs, wvs) = zipWith (+) bs $ sum . zipWith (*) as <$> wvs
    
    feed :: [Float] -> [([Float], [[Float]])] -> [Float]
    feed = foldl' (((relu <$>) . ) . zLayer)
    
    newBrain :: [Int] -> [([Float], [[Float]])]
    newBrain szs@(_:ts) = zip (flip replicate 1 <$> ts) <$> zipWithM (\m n-> replicateM n $ replicateM m $ gauss 0.01 ) szs ts