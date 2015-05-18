{-# LANGUAGE ViewPatterns #-}
module ZCompiler where
import Data.Functor
import Control.Applicative
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.Array.ST  
import Data.List (take,drop,sortBy)
import System.Environment
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

isSorted :: Ord a => [a] -> [a] -> Bool
isSorted xs ys = ((sortBy (flip compare) xs) == ys) 

arraysize = 50000

changeTo :: Int -> [Int] -> [Int]
changeTo n xs = take n $ xs ++ replicate n 0 

isValid :: Code -> Bool
isValid code = (not $ null code) && (last code == Done) && 
               (all (uncurry inside) (zip [0..] code))
  where
    inside i op = case op of 
        (J a b m n) -> ((checkBounds l [i+m,i+n]) && (checkBounds arraysize [a,b]))
        (I a)       -> checkBounds arraysize [a]
        (Z a)       -> checkBounds arraysize [a]
        Done        -> True
    checkBounds ele xs = and ([(>=0),(<ele)] <*> xs) 
    l = length code

main :: IO ()
main = do
  m           <- readLn :: IO Int
  initialRAM  <- (map readInt . words) <$> getLine
  n           <- readLn :: IO Int
  code        <- (sequence . replicate n) (readLn :: IO Op)
  let compile = isValid code
      endRAM  = eval (changeTo arraysize initialRAM) code
      finalA  = take (head initialRAM) (tail endRAM)
      correct = compile && isSorted (take (head initialRAM) (tail initialRAM)) finalA
  unless compile (error "Programa no compila.")
  putStrLn $ (unwords.map show.take m) endRAM
  (print correct >> getLine >> putStr "")
