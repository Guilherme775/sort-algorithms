module Sort (sort, msort, qsort, isort) where

sort :: Ord a => [a] -> [a]
sort = msort

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort list = merge (msort xs) (msort ys)
  where
    (xs, ys) = splitAt midpoint list
    midpoint = length list `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge xs'@(x : xs) ys'@(y : ys)
  | x < y = x : merge xs ys'
  | otherwise = y : merge xs' ys

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (y : ys) = qsort small ++ [y] ++ qsort large
  where
    small = filter (< y) ys
    large = filter (>= y) ys

isort :: Ord a => [a] -> [a]
isort [] = []
isort [x] = [x]
isort (x : xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x ys'@(y : ys)
  | x > y = y : insert x ys
  | otherwise = x : ys'
