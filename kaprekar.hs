-- Kaprekar Routine Visualization
-- By: andrus@uchicago.edu
-- Inspiration: http://mathworld.wolfram.com/KaprekarRoutine.html
import GHC.Ptr
import Data.Maybe
import Data.Word
import qualified Data.List as List
import Data.Digits
import qualified Data.Vector.Storable as V
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.ByteString

{- Kaprekar Functions -}

mkMaxMin base width n = (unDigits base $ List.reverse m, unDigits base m)
    where m = replicate (width - length l) 0 ++ l
          l = List.sort $ digitsRev base n

genPairs base width = List.map (mkMaxMin base width . unDigits base) $ f width (base-1) where
    f 0 _ = [[]]
    f k m = do a <- [0..m]
               ax <- f (k-1) a
               if k==width && all (a==) ax then []
                                           else return $! a:ax

subPairs = List.map (uncurry (-))

regenPairs base width = List.map (mkMaxMin base width)

iterations base width xs | all (uncurry (==)) $ zip is js = ys
                         | otherwise = iterations base width ys
     where ys = zip js qs
           js = List.map (\(i, p, q) -> if p==q then i else i+1) $ zip3 is ps qs
           qs = subPairs $ regenPairs base width ps
           (is, ps) = unzip xs
 
lookupCoord base width n kvs | isNothing v = -1
                             | otherwise  = fromJust v where v = lookup (mkMaxMin base width n) kvs

{- Kaprekar Functions' -}

iterator base width np | n==p = (i, n)
                       | otherwise = iterator base width (i+1, next n, n)
    where (i, n, p) = np
          next k = uncurry (-) $! mkMaxMin base width k

{- Image Functions -}

isqrt = floor . sqrt . fromIntegral

-- TODO: rewrite this so we can accomodate numbers iteration steps larger than 6.
v :: [(Int, Int)] -> V.Vector Word8
v ps = V.fromList . concat $ List.map (c . snd) ps
     where c (-1) = [255, 255,   0, 255] -- Yellow
           c   0  = [128, 255,   0, 255] -- Yellow-Green
           c   1  = [  0, 255,  64, 255] -- Green
           c   2  = [  0, 255, 255, 255] -- Cyan
           c   3  = [  0,  64, 255, 255] -- Blue
           c   4  = [128,   0, 255, 255] -- Purple
           c   5  = [255,   0, 192, 255] -- Magenta
           c   6  = [255,   0,   0, 255] -- Red
           c   _  = [  0,   0,   0, 255] -- Black

ptr2repa i j k = copyFromPtrWord8 (Z :. i :. j :. k)

{- Main -}

main :: IO ()
main = do let (base, width) = (10, 4)
          let !dim = base^width
          let (i, j, k) = (isqrt dim, isqrt dim, 4 {-RGBA-})
          let ps = genPairs base width
          --let !qs = iterations base width . zip (replicate (length ps) 0) $ subPairs ps
          let !qs = List.map (iterator base width) $ zip3 (replicate (length ps) 0) (subPairs ps) (replicate (length ps) 0)
          let kvs = zip ps (List.map fst qs)
          let vs = v [(n, lookupCoord base width n kvs) | n <- [0..dim-1]]
          r <- V.unsafeWith vs $! ptr2repa i j k
          runIL $ writeImage "kaprekar.bmp" r
