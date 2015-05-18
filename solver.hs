{-# LANGUAGE ViewPatterns #-}
-------------
--Evaluador--
------------
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.Array.ST
import Data.List (take,drop)
import qualified Data.Array as A
import qualified Data.Vector as V
data Op = Z Int | I Int | J Int Int Int Int | Done deriving (Show,Read,Eq)
type Code = [Op]
type RAM s  = STArray s Int Int

readInt :: String -> Int
readInt = read

readOp :: String -> Op
readOp = read

modifyArray :: (MArray a e m, Ix i) => a i e -> i -> (e -> e) -> m ()
modifyArray arr idx fn = do
   val <- readArray arr idx
   writeArray arr idx (fn val)

while :: Monad m => m Bool -> m a -> m ()
while cond act = do
  b <- cond
  when b $ do
    act
    while cond act

operate :: Op -> STRef s Int -> (RAM s) -> ST s ()
operate (Z i) ptr arr       = (modifyArray arr i (const 0)) >> (modifySTRef ptr succ)
operate (I i) ptr arr       = (modifyArray arr i succ) >> (modifySTRef ptr succ)
operate (J a b m n) ptr arr = do
  x <- readArray arr a
  y <- readArray arr b
  modifySTRef ptr (if x == y then (+ m) else (+ n))
operate Done        _   _   = return ()

arraysize' = 50000

isValid :: Code -> Bool
isValid code = (not $ null code) && (last code == Done) && 
               (all (uncurry inside) (zip [0..] code))
  where
    inside i op = case op of
        (J a b m n) -> checkBounds l [i+m,i+n,a,b]
        (I a)       -> checkBounds arraysize' [a]
        (Z a)       -> checkBounds arraysize' [a]
        Done        -> True
    checkBounds ele xs = and ([(>=0),(<ele)] <*> xs)
    l = length code


changeTo :: Int -> [Int] -> [Int]
changeTo n xs = take n $ xs ++ replicate n 0

eval :: [Int] -> Code -> [Int]
eval ram (V.fromList -> code) = A.elems $ runSTArray $ do
  arr <- newListArray (0,length ram - 1) ram
  ptr <- newSTRef (0 :: Int)
  while (((Done /=) . readOp) <$> (readSTRef ptr)) $ do
    op <- readOp <$> (readSTRef ptr)
    operate op ptr arr
  return arr
    where
      readOp p = code V.! p

eval' :: Int -> Code -> [Int]
eval' n = eval (replicate n 0)

------------
--Solucion--
------------

copia :: Int -> Int -> Code
copia a b = [Z b,J a b 3 1, I b, J a b 1 (-1)]

minimos :: [Int] -> Int -> Code
minimos xs m =
  map (\(i,n)-> J n 0 (l-i+m) 1) (zip [0..] xs) ++ 
  map I [0..m] ++ 
  map (\(i,n)-> J n 0 (l+i) 1) (zip [0..] xs) ++ 
  [J 0 0 (-l-m) 0] ++
  concatMap (\(i,n)-> [Z n , J 0 0 (-1 + 2*(l-1-i)) 1]) (zip [0..] xs)
    where l = length xs + 1

countZeros :: Int -> Int -> Code
countZeros df cuan = 
  [Z 0] ++ 
  concatMap (\i -> map (\_ -> J (i+df) 0 (i+1) i) [1..i]) [1..cuan]

move :: Int -> Code
move cuan = concatMap (\n -> copia n (n+cuan)) [1..cuan]

clear :: Int -> Code
clear cuan = map Z [0..cuan]

sorting :: Int -> Code
sorting cuan = 
  move cuan ++
  clear cuan ++
  countZeros cuan cuan ++
  map (\s -> J 0 0 s 0) saltos ++
  concatMap (\i -> [J 0 0 (i+2) 0] ++ map I [0..i] ++ minimos [cuan+1..2*cuan] i) [cuan,cuan-1..1] ++
  [Done,Done,Done] 
    where
      saltos = scanl (+) (cuan+2) (map (\i -> 4*cuan+2+(cuan+1-i)+1+(cuan+1-i)) [1..cuan])

