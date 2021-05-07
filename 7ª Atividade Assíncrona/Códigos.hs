--EX 1

paridade :: [Int] -> [Bool]
paridade l = map (even) l

--EX 2

prefixos :: [String] -> [String]
prefixos l = map (take 3) l

--EX 3

saudacao :: [String] -> [String]
saudacao l =  map ( ++ "Oi ") l

--EX 4

filtrar :: (a -> Bool) -> [a] -> [a]
filtrar f l = [x | x <- l, f x]

--EX 5

pares :: [Int] -> [Int]
pares lst = filter (even) lst

--EX 6

solucoes :: [Int] -> [Int]
solucoes l = filter (\x -> ((5*x+6) < (x*x))) l

--EX 7

maior :: [Int] -> Int
maior [ ] = 0
maior l = foldr1 (max) l

--EX 8

menor_min10 :: [Int] -> Int
menor_min10 [ ] = 0
menor_min10 l = foldr (min) 10 l

--EX 9

junta_silabasplural :: [String] -> String
junta_silabasplural l = foldr (++) "s" l

--EX 10

menores10 :: [Int] -> ([Int], Int)
menores10 l = (x, length x)
 where x = [ x | x <- l, x < 10]

--EX 11

busca :: Int -> [Int] -> Int -> (Bool, Int)
busca _ [ ] c = (False, c - 1)
busca n (x:xs) c
  | n /= x = busca n xs (c + 1)
  | otherwise = (True, c)

busca_elem :: Int -> [Int] -> (Bool, Int)
busca_elem n (x : r) = busca n (x : r) 1