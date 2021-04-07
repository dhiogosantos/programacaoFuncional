--EX 1

conta_ch :: [Char] -> Integer
conta_ch [ ] = 0
conta_ch (h : t) = 1 + conta_ch t

conta :: [a] -> Integer
conta [ ] = 0
conta (h : t) = 1 + conta t

maior :: [Int] -> Int
maior [x] = x
maior (h : t : tt)
  | h > t = maior (h : tt)
  | otherwise = maior (t : tt)

primeiros :: Int -> [a] -> [a]
primeiros _ [] = []
primeiros 0 _ = []
primeiros n (h : t) = h : primeiros (n - 1) t

pertence :: Eq a => a -> [a] -> Bool
pertence a [] = False
pertence a (h : t) = if (a == h) then True else pertence a t

uniaoR :: Eq a => [a] -> [a] -> [a]
uniaoR [] l = l
uniaoR (h : t) l
  | pertence h l = uniaoR t l
  | otherwise = h : uniaoR t l

--EX 2

npares :: [Int] -> Int
npares [ ] = 0
npares (h : t)
 | even h = 1 + npares t
 | otherwise = npares t

--EX 3

produtorio :: [Float] -> Float
produtorio [ ] = 0
produtorio [h] = h
produtorio (h : t) = h * produtorio t

--EX 4

comprime :: [[a]] -> [a]
comprime [ [ ] ] = [ ]
comprime ([ ] : t) = comprime t 
comprime ( (h : t) : x) = h : comprime (t : x)

--EX 5

tamanho :: [a] -> Int
tamanho [ ] = 0
tamanho [a] = 1
tamanho (h : t) = 1 + tamanho t

--EX 6

compara :: Int -> [Int] -> [Int]
compara x [ ] = [ ]
compara x (h : t) 
 |x == h = compara x t 
 |otherwise = h : compara x t 

uniaoRec2 :: [Int] -> [Int] -> [Int]
uniaoRec2 [ ] [ ] = [ ]
uniaoRec2 [ ] y = y
uniaoRec2 [x] y = x : compara x y
uniaoRec2 (h : t) y = h : uniaoRec2 t (compara h y)

