--EX 1

--a)

--(||) :: Bool -> Bool -> Bool
--_ || True = True
--True || _ = True
--False || False = False

--(||) :: Bool -> Bool -> Bool
-- True  || _ = True
-- False || x = x

--(||) :: Bool -> Bool -> Bool
-- False || False = False
-- _ || _     = True

--b)

--(||) :: (Bool -> Bool) -> Bool
-- (x,y)
--   | True  _ = True 
--   | False  False = False
--   |  _  True = True


-- (||) :: Bool -> Bool -> Bool
--  (x y)  = if (True || _ )
--	then True 
-- 	else if False || y then y

--EX 2

type Ponto = (Float, Float)

distancia :: Ponto -> Ponto -> Float
distancia (a,b) (c,d) = sqrt (((c-a)^2) + ((d-b)^2))

--EX 3

fatorialg :: Int -> Int
fatorialg n 
 | n == 0 = 1
 | n == 1 = 1
 | otherwise = n * fatorialg (n - 1)

fatorialcp :: Int -> Int 
fatorialcp 0 = 1
fatorialcp x = x * fatorialcp (x - 1)

--EX 4 

fibo :: Int -> Int
fibo x
 | x == 1 = 1
 | x == 2 = 1
 | otherwise = fibo (x - 2) + fibo (x -1)

--EX 5

n_tri :: Int -> Int
n_tri x
 | x == 0 = 0
 | x == 1 = 1
 | otherwise = n_tri (x - 1) + x

--EX 6

potencia2 :: Int -> Int
potencia2 0 = 1
potencia2 1 = 2
potencia2 x = 2 * potencia2 (x - 1)

--EX 7

--a)

prodIntervalo :: Int -> Int -> Int
prodIntervalo m n 
 | m >= n = n
 | otherwise = m * prodIntervalo (m + 1) n

--b)

fat_intervalo :: Int -> Int
fat_intervalo 0 = 1
fat_intervalo x = prodIntervalo 1 x

--EX 8

resto_div :: Int -> Int -> Int
resto_div m n 
  | m < n = m
  | otherwise = resto_div (m - n) n

div_inteira :: Int -> Int -> Int
div_inteira m n
  | m < n = 0
  | otherwise = 1 + div_inteira (m - n) n

--EX 9

mdc :: (Int, Int) -> Int
mdc (m, 0) = m
mdc (m, n) = mdc (n, (mod m n))

mdcg :: (Int, Int) -> Int
mdcg (m, n)
 | n == 0 = m
 | otherwise = mdcg (n, (mod m n))

--EX 10

binomialg :: (Int, Int) -> Int
binomialg (n, 0) = 1
binomialg (n, k)
 | k == 0 = 1
 | k == n = 1
 | otherwise = binomialg (n - 1, k) + binomialg (n - 1, k - 1)

binomial :: (Int, Int) -> Int
binomial (n, 0) = 1
binomial (n, k) = if (k == n) then 1 else binomial (n - 1, k) + binomial (n - 1, k - 1)

