--EX 1

analisa_raizes :: Float -> Float -> Float -> String
analisa_raizes a b c
 | b ^  2  >  4  * a * c = " 1) Possui duas raizes reais "
 | b ^  2  ==  4  * a * c = " 2) Possui uma raiz real "
 | b ^  2  <  4  * a * c = " 3) Nenhuma raiz real "
 | a ==  0  =  " 4) Equacao degenerada "

--EX 2 

delta :: (Float , Float , Float) ->  Float
delta (a, b, c) =  sqrt (b ^  2  -  4  * a * c)

equacao :: Float -> Float -> Float -> (Float, Float)
equacao a b c
 | a == 0  = (( - c) / b, a)
 | otherwise  = ((- b + delta (a, b, c)) / (2  * a), (- b - delta (a, b, c)) / ( 2  * a))

--EX 3

{-

tipo  Data  = (Int, Int, Int)

valor_passagem :: Float -> Data -> Data -> Float
valor_passagem valor_total (dA, mA, aA) (dN, mN, aN) 
 |

-}

--EX 4

type Lista  = [Int]

--a)
gera1 :: Lista
gera1 = [n ^ 2  | n <- [ 1 .. 15 ], n >  3 , n <  15 , mod n 2  /=  0 ]

--b)
gera2 :: [( Int , Int )]
gera2 = [(x, y) | x <- [ 1 .. 15 ], x <  5 , y <- [x .. x + x]]

--c)
gera3 :: Lista
gera3 = [n | x <- [ 1 .. 15 ], x >  9 , n <- [ 1 .. x]]

--d)
gera4 :: [( Int , Int )]
gera4 = [(n, n + 1 ) | n <- [ 1 .. 15 ], even n]

--e)
gera5 :: Lista
gera5 = [ fst (n) +  snd (n) | n <- gera4]

--EX 5

--a) 

contaNegM2 :: [Int] -> Int
contaNegM2 lista = sum [ 1  | x <- lista, mod x 3  ==  0 , x > 0]

--b) 

listaNegM2 :: [Int] -> [Int]
listaNegM2 lista = [ x | x <- lista, mod x 3  ==  0 , x >  0]

--EX 6

fatores :: Int -> [Int]
fatores num = [y | y <- [1..num], mod num y == 0]

primos :: Int -> Int -> [Int]
primos numA numB = [ x | x <- [numA..numB], length (fatores x) == 2 ]

--EX 7

mmc :: Int -> Int -> Int -> Int 
mmc a b c = head [ x | x <- [1..(a*b*c)], (mod x a == 0) && (mod x b == 0) && (mod x c == 0)]

--EX 8

--EX 9

fizzbuzz :: Int -> [String]
fizzbuzz 1 = ["No"]
fizzbuzz n
 | (mod n 2 == 0) && (mod n 3 == 0) = fizzbuzz (n-1) ++ ["FizzBuzz"]
 | mod n 2 == 0 = fizzbuzz (n-1) ++ ["Fizz"]
 | mod n 3 == 0 = fizzbuzz (n-1) ++ ["Buzz"]
 | otherwise = fizzbuzz (n-1) ++ ["No"]

--EX 10

seleciona_multiplos :: Int -> [Int] -> [Int]
seleciona_multiplos n [ ] = [ ]
seleciona_multiplos n t = [ x | x <- t, mod x n == 0]

--EX 11

unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia n (x : r)
 | not (elem n (x : r)) = False
 | (n == x) && elem n r = False
 | (n == x) && not (elem n r) = True
 | otherwise = unica_ocorrencia n r

--EX 12

intercala :: [ a ] -> [ a ] -> [ a ]
intercala [ ]  [ ]  =  [ ]
intercala x [ ]  = x
intercala [ ] y = y
intercala (x : xs) (y : ys) = [x, y] ++ (intercala xs ys)

--EX 13

zipar :: [ a ] -> [ a ] -> [ [ a ] ]
zipar [ ]  [ ]  =  [ ]
zipar x [ ]  =  [ ]
zipar [ ] y =  [ ]
zipar (x : xs) (y : ys) = [x, y] : zipar xs ys

--EX 14

type Contato = (String, String, String, String)
type Agenda = [Contato]

agenda1 :: Agenda
agenda1 = 
 [( "Fulano", "Rua Abacaxi", "01234567", "fulano@ufu.br"),
  ( "Ciclano", "Rua Cebola", "00123456", "ciclano@ufu.br"),
  ("Beltrano", "Rua Laranja", "00012345", "beltrano@ufu.br")]

recuperar :: String -> Agenda -> String
recuperar email [_] = "Email desconhecido"
recuperar email ((nome1, _, _, email1) : r) = if (email1 == email)
then nome1
else recuperar email r

--EX 15

{-

type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]

pessoas = [( " Rosa " , 1,66 , 27 , 'C'),
  ( " Joao " , 1,85 , 26 , 'C'),
  ( " Maria " , 1,55 , 62 , 'S'),
  ( " Jose " , 1,78 , 42 , 'C'),
  ( " Paulo " , 1,93 , 25 , 'S'),
  ( " Clara " , 1,70 , 33 , 'C'),
  ( " Bob " , 1,45 , 21 , 'C'),
  ( " Rosana " , 1,58 , 39 , 'S'),
  ( " Daniel " , 1,74 , 72 , 'S'),
  ( " Jocileide " , 1,69 , 18 , 'S')]


altura_media_aux  :: [ Pessoa ] ->  Float ->  Float ->  Float
altura_media_aux ((_, alturaPessoa, _, _) : xs) soma tamanho = altura_media_aux xs (soma + alturaPessoa) (tamanho +  1 )
altura_media_aux [ ] soma tamanho = soma / tamanho

altura_media  :: [ Pessoa ] ->  Float
altura_media x = altura_media_aux x 0  0

idade_menor  :: [ Pessoa ] ->  Int
idade_menor [ ]  =  0
idade_menor [(_, _, x, _)] = x
idade_menor ((x1, x2, x, x3) : (y1, y2, y, y3) : ys)
 | x1 < y1 = idade_menor ((x1, x2, x, x3) : ys)
 | otherwise  = idade_menor ((y1, y2, y, y3) : ys)

mais_velho :: [Pessoa] -> (String, Char)
mais_velho [(nome,  _,  _,  civil)] = (nome, civil)
mais_velho ((nome1, altura1, idade1, civil1) : (nome2, altura2, idade2, civil2) : ys)
 | idade1 > idade2 = mais_velho ((nome1, altura1, idade1, civil1) : ys)
 | otherwise = mais_velho ((nome2, altura2, idade2, civil2) : ys)

maior_cinq :: [Pessoa] -> [Pessoa]
maior_cinq p = [(nome, altura, idade, civil) | (nome, altura, idade, civil) <- p, idade > =  50 ]

casadas_aux  :: [Pessoa] -> Int -> [Pessoa]
casadas_aux pi = [(nome, altura, idade, civil) | (nome, altura, idade, civil) <- p, idade > = i, civil ==  'C' ]

casadas :: [Pessoa] -> Int  -> Int
casadas pi =  comprimento (casadas_aux pi)

-}
 
--EX 16

insere_ord  ::  Ord  a  =>  a  -> [ a ] -> [ a ]
insere_ord x [ ]  = [x]
insere_ord x (y : ys)
 | x < y = (x : y : ys)
 | otherwise  = y : (insere_ord x ys)

--EX 17

reverte  :: [ a ] -> [ a ]
reverte [ ]  =  [ ]
reverte (x : xs) = (reverte xs) ++ [x]

--EX 18

elimina_repet  ::  Eq  a  => [ a ] -> [ a ]
elimina_repet [ ]  =  [ ]
elimina_repet (x : xs)
 | elem x xs = elimina_repet xs
 | otherwise  = x : elimina_repet xs

--EX 19

disponiveis = [1, 2, 5, 10, 20, 50, 100]

notasTroco  ::  Int -> [ [Int] ]
notasTroco 0  = [ [ ] ]
notasTroco x = [ [e] ++ l | e <- disponiveis, e <= x, l <- notasTroco (x - e)]

--EX 20

