--EX 2

--a) 

a = [5, 4 .. 1]

--b)

b = ['a', 'c' .. 'e']

--c)

c = [1, 4 .. 16]

--d)

d = zip [1, -2 .. -11] [1, 5 .. 17]

--EX 3

--a)

intervalo :: Int -> Int -> [Int]
intervalo a b = [a .. b] 

--b) intervalo2 :: Int -> Int -> [Int]
intervalo2 a b
 | a == b = [ ] 
 | otherwise = [ x | x <- [a .. b], even x]

--EX 4

lst1 = [x*2 | x <- [1..10], x*2 >= 12]
lst2 = [ x | x <- [50..100], mod x 7 == 3]
lst3 = [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]
lst4=[(x,y)| x <- [1..4], y <- [x..5]]

--EX 5

quadrados :: Int -> Int -> [Int]
quadrados x y = [ a*a | a <- [x .. y]]

--EX 6

seleciona_impares :: [Int] -> [Int]
seleciona_impares t = [x | x <- t, odd x]

--EX 7

tabuada :: Int -> [Int]
tabuada x = [ x*y | y <- [1 .. 10]]

--EX 8

bissexto :: Int -> Bool
bissexto x
  | mod x 400 == 0 = True
  | (mod x 4 == 0) && (mod x 100 /= 0) = True
  | otherwise = False

bissextos :: [Int] -> [Int]
bissextos z = [x | x <- z, bissexto x]

--EX 9

sublistas :: [[Int]] -> [Int]
sublistas x = [y | y2 <- x, y <- y2]

--EX 10

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos

bdEmprestimo =
 [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"),
 ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"),
 ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (d1, m1, a1) (d2, m2, a2) | a1 < a2 = True
 | a1 == a2 && m1 < m2 = True
 | a1 == a2 && m1 == m2 && d1 < d2 = True
 | otherwise = False

data_dev :: Emprestimo -> Data
data_dev (_, _, _, dt, _) = dt

atrasados :: Emprestimos -> Data -> Emprestimos
atrasados emp data_atual = [n | n <- emp, precede (data_dev n) data_atual]

--EX 11

uniaoNRec :: [Int] -> [Int] -> [Int]
uniaoNRec a b = [n | x <- [a, b], n <- x]
