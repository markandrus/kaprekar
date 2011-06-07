-- Kaprekar Routine Visualization
-- By: andrus@uchicago.edu
-- Inspiration: http://mathworld.wolfram.com/KaprekarRoutine.html
import Data.Maybe
import Data.Word
import qualified List
import Data.Digits
import qualified Data.Vector.Storable as V
import Data.Array.Repa hiding ((++))
import Data.Array.Repa.IO.DevIL
import Data.Array.Repa.ByteString


{- Kaprekar Functions -}

mk_max_min :: Int -> (Int, Int)
mk_max_min n = (unDigits 10 $ List.reverse m, unDigits 10 $ m)
    where m = (replicate (4 - length l) 0) ++ l
          l = List.sort $ digitsRev 10 n


gen_pairs = [(1000*a + 100*b + 10*c + 1*d, 1000*d + 100*c + 10*b + a)
             | a <- [0..9]
             , b <- [0..a]
             , c <- [0..b]
             , d <- [0..c]
             , not $ all (a==) [a, b, c, d]]


sub_pairs ps = List.map (\(max, min) -> max - min) ps


regen_pairs ps = filter (\(x, y) -> x /= y) --is this line necessary?
               $ List.map (mk_max_min) ps


iterations xs | (length $ List.nub qs) > 1 = iterations ys
              | otherwise = ys
     where ys = zip js qs
           js = List.map (\(i, (p, q)) -> if (p==q) then i else i+1) . zip is $ zip ps qs
           qs = sub_pairs $ regen_pairs ps
           (is, ps) = unzip xs
 

lookup_coord :: Int -> [((Int, Int), Int)] -> Int
lookup_coord n kvs | isNothing v  = (-1)
                   | otherwise    = fromJust v where v = lookup (mk_max_min n) kvs


{- Image Functions -}

i, j, k :: Int
(i, j, k) = (100, 100, 4 {-RGBA-})

v :: [(Int, Int)] -> V.Vector Word8
v ps = V.fromList . take (i * j * k) . concat $ List.map (to_pallette . snd) ps
     where to_pallette n | n == (-1) = [255, 255,   0, 255] -- Yellow
                         | n ==   0  = [128, 255,   0, 255] -- Yellow-Green
                         | n ==   1  = [  0, 255,  64, 255] -- Green
                         | n ==   2  = [  0, 255, 255, 255] -- Cyan
                         | n ==   3  = [  0,  64, 255, 255] -- Blue
                         | n ==   4  = [128,   0, 255, 255] -- Purple
                         | n ==   5  = [255,   0, 192, 255] -- Magenta
                         | n ==   6  = [255,   0,   0, 255] -- Red
                         | otherwise = [  0,   0,   0, 255] -- Black


ptr2repa p = copyFromPtrWord8 (Z :. i :. j :. k) p


{- Main -}

main = do let ps = gen_pairs
          let qs = iterations . zip (replicate (length ps) 0) $ sub_pairs ps
          let kvs = zip ps (List.map (fst) qs)
          let vs = v $ [(n, lookup_coord n kvs) | n <- [0..9999]]
          r <- V.unsafeWith vs ptr2repa
          runIL $ writeImage "kaprekar.png" r
