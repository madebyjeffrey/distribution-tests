module Normal (normalSample) where

import qualified Data.Sequence as Seq

normalDistribution :: Double -> Double -> Double -> Double
normalDistribution µ σ x = exp (-(exponent_top / exponent_bottom)) / denominator
  where denominator = sqrt $ 2 * pi * σ * σ
        exponent_top = (x - µ) ^ (2 :: Integer)
        exponent_bottom = 2 * σ * σ

normalPdf :: Double -> Double
normalPdf = normalDistribution 80.0 20.0

normalPdfInt :: Int -> Double
normalPdfInt x = minY + (maxY - minY) / 2.0
  where x0 = fromIntegral x
        x1 = fromIntegral x + 1
        y0 = normalPdf x0
        y1 = normalPdf x1
        minY = min y0 y1
        maxY = max y0 y1

normalPdfRange :: [Double]
normalPdfRange = normalPdfInt <$> take 126 [0..]

normalizedPdf :: [Double]
normalizedPdf = (/ maximal) <$> normalPdfRange
  where maximal = foldr max 0 normalPdfRange

fixedPdfRange :: [Double]
fixedPdfRange = (0.6 <$ [(0::Int)..29]) <> drop 30 normalizedPdf

mappedPdf :: Seq.Seq Double
mappedPdf = Seq.fromList fixedPdfRange 

normalSample :: Int -> Double 
normalSample x = case Seq.lookup x mappedPdf of 
    Just y -> y 
    Nothing -> 0.0