-- Kaprekar Routine Visualization
-- By: andrus@uchicago.edu
-- Inspiration: http://mathworld.wolfram.com/KaprekarRoutine.html
import Data.Maybe
import Data.Word
import qualified Data.List as List
import Data.Digits
import qualified Data.Vector.Storable as V
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.ByteString

base = 10
width = 4
numbers = base^width

{- Kaprekar Functions -}

mkMaxMin :: Int -> (Int, Int)
mkMaxMin n = (unDigits base $ List.reverse m, unDigits base m)
    where m = replicate (width - length l) 0 ++ l
          l = List.sort $ digitsRev base n

genPairs = List.map (mkMaxMin . unDigits base) $ f width (base-1) where
    f 0 _ = [[]]
    f k m = do a <- [0..m]
               ax <- f (k-1) a
               if k==width && all (a==) ax then []
                                           else return $! a:ax

subPairs = List.map (uncurry (-))

regenPairs = List.map mkMaxMin

iterations xs | all (uncurry (==)) $ zip is js = ys
              | otherwise = iterations ys
     where ys = zip js qs
           js = List.map (\(i, p, q) -> if p==q then i else i+1) $ zip3 is ps qs
           qs = subPairs $ regenPairs ps
           (is, ps) = unzip xs
 
lookupCoord n kvs | isNothing v = -1
                  | otherwise  = fromJust v where v = lookup (mkMaxMin n) kvs

{- Image Functions -}

isqrt = floor . sqrt . fromIntegral

(i, j, k) = (isqrt numbers, isqrt numbers, 4 {-RGBA-})

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

ptr2repa = copyFromPtrWord8 (Z :. i :. j :. k)

{- Main -}

main = do let !ps = genPairs
          let !qs = iterations . zip (replicate (length ps) 0) $ subPairs ps
          let kvs = zip ps (List.map fst qs)
          let vs = v [(n, lookupCoord n kvs) | n <- [0..numbers-1]]
          r <- V.unsafeWith vs ptr2repa
          runIL $ writeImage "kaprekar.bmp" r
