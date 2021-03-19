--EX 1

dobro :: Float -> Float
dobro x = x*2

quadruplicar :: Float -> Float
quadruplicar x = (dobro x) + (dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt ((x^2) + (y^2))

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (a,b) (c,d) = sqrt (((c-a)^2) + ((d-b)^2))

--EX 3

conversao :: Float -> (Float , Float , Float)
conversao r = (r, (r*3.96), (r*4.95))

--EX 4

bissexto :: Int -> Bool
bissexto x | (mod x 400 == 0) = True
 | (mod x 4 == 0) && (mod x 100 /= 0) = True
 | otherwise = False  

--EX 5

type Data = (Int, Int, Int)

bissexto2 :: Data -> Bool
bissexto2 (d, m, x) = (bissexto x)

--EX 6

valida :: Data -> Bool
valida (d,m,x) | (d >= 1 && d <= 31) && (m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12) = True
 | (d >= 1 && d <= 30) && (m == 4 || m == 6 || m == 9 || m == 11) = True
 | (d >= 1 && d <= 28) && (m == 2) && (not (bissexto x)) = True
 | (d >= 1 && d <= 29) && (m == 2) && (bissexto x) = True
 | otherwise = False

--EX 7

precede :: Data -> Data -> Bool
precede (d, m, a) (d1, m1, a1) | (valida (d, m, a) == True) && (valida (d1, m1, a1) == True) && (a < a1) = True
 | (a > a1) = False
 | (valida (d, m, a) == True) && (valida (d1, m1, a1) == True) && (m < m1) = True
 | (m > m1) = False
 | (valida (d, m, a) == True) && (valida (d1, m1, a1) == True) && (d < d1) = True
 | otherwise = False

--EX 8

type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, String)
type Emprestimo = (String, String, Data, Data, String)


--EX 9

--e1 :: Emprestimo
--verifica :: Data -> e1-> String
--verifica (a,b,c) (codLivro, codAluno, (d, m, a), (d1, m1, a1), status) 
--| (precede (valida ((de, me, ae), (dd, md, ad)))) == True) = "Emprestimo em dia."
--| (precede (valida ((de, me, ae), (dd, md, ad)))) ==False) = "Emprestimo atrasado."
--| otherwise = "False"
