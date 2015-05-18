module Zorting where
data Op = Z Int | I Int | J Int Int Int Int | Done deriving (Show,Read,Eq)
type Code = [Op]

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

