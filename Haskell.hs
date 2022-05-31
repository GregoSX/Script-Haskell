-- Instalar no WSL com sudo apt-get install haskell-platform
-- Abrir o terminal digitar ghci, depois digite :l nomeDoArquivo, depois só chamar a função

-- 1) Menor Entre dois numeros
menorDeDois :: (Ord t) => t -> t -> t
menorDeDois a b
    | a < b = a
    | otherwise = b

-- 2) Menor Entre tres numeros
menorDeTres :: (Ord t) => t -> t -> t -> t
menorDeTres a b c = menorDeDois a(menorDeDois b c)

-- 3) Calcula o fatorial de um numero
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial n = n * fatorial(n-1)

-- 4) Recebe um número inteiro positivo e retorna o n-ésimo elemento da sequência de Fibonacci
fibonacci :: Integer -> Integer 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci (n-2)

-- 5) Retorna o n-ésimo termo de uma lista
elemento :: Int -> [t] -> t
elemento i (c:r)
   | i == 1 = c
   | otherwise = elemento(i-1)r

-- 6) Retorna True se um elemento pertence a uma lista
pertence :: (Eq t) => t -> [t] -> Bool
pertence _[] = False 
pertence elem (c:r)
    | elem == c = True
    | otherwise = pertence elem r

-- 7) Recebe uma lista qualquer e retorna o número de elementos na lista
nroElementos :: [t] -> Int
nroElementos [] = 0
nroElementos (c:r) = 1 + nroElementos r

-- 8) Recebe uma lista e retorna o maior elemento
maior :: (Ord t) => [t] -> t
maior [c] = c
maior (c:r)
    | (c > (maior r)) = c
    | otherwise = maior r

-- 9) Recebe um elemento de uma lista qualquer, retorna o número de ocorrências do elemento na lista
nroOcorrencias :: (Eq t) => t -> [t] -> Int
nroOcorrencias a [] = 0
nroOcorrencias a (c:r)
    | (a == c) = 1 + nroOcorrencias a r
    | otherwise = nroOcorrencias a r


-- 10) Recebe um elemento e uma lista e verifica se existe uma única ocorrência do elemento na lista
unicaOcorrencia:: (Eq t) => t -> [t] -> Bool
unicaOcorrencia a (c:r) 
    | (nroOcorrencias a (c:r) == 1) = True
    | otherwise = False


-- 11) Retorna os elementos de uma lista que são maiores que um numero fornecido
maiores_que:: (Ord t) => t -> [t] -> [t]
maiores_que _[] = []
maiores_que x (c:r) 
    | (c > x) = c : maiores_que x r
    | otherwise = maiores_que x r

-- 12.1) Concatena duas listas
concatena :: [t] -> [t] -> [t]
concatena [] l = l
concatena (c:r) l = c : (concatena r l)


-- 12.2) Concatena duas listas sem numeros repetidos
concatenaA :: (Eq t) => [t] -> [t] -> [t]
concatenaA as bs = as ++ [b | b <- bs, not (pertence b as)]

-- 13) Recebe um elemento e uma lista e retorna a lista sem a primeira ocorrência do elemento
remove :: (Eq t) => t -> [t] -> [t]
remove x (c:r)
    | (x == c) = r
    | otherwise = c : (remove x r)

-- 14) Recebe uma lista e retorna a lista sem o último elemento
removerUltimo :: [t] -> [t]
removerUltimo [c] = []
removerUltimo (c:r) = c : (removerUltimo r)

-- 15.1) Recebe uma lista e retorna outra lista sem repetição de elementos
removerRepetidos :: (Eq t) => [t] -> [t]
removerRepetidos [] = []
removerRepetidos [c] = [c]
removerRepetidos (c:r) = c : (removerRepetidos(removerTodos c r))

-- 15.2) Remove todos os elementos repetidos em uma lista
removerTodos :: (Eq t) => t -> [t] -> [t]
removerTodos _ [] = []
removerTodos e (c:r)
    | e == c = removerTodos e r
    | otherwise = c : (removerTodos e r)

-- 16.1) Recebe um número n e uma lista, e retorna uma lista com os n elementos maiores da lista
maiores :: (Ord t) => Int -> [t] -> [t]
maiores n l@(c:r)
    | n >= (nroElementos l) = l
    | otherwise = maiores n(removerUltimoMenor l)

-- 16.2) Recebe uma lista, e retorna o menor elemento
menor :: (Ord t) => [t] -> t
menor [e] = e
menor (c:r) = menorDeDois c (menor r)

-- 16.3) Remove o ultimo menor elemento da lista
removerUltimoMenor l = (remove (menor l) (reverse l))

-- 17) Recebe um número inteiro n positivo e retorna a lista
geraSequencia :: Int -> [Int]
geraSequencia 1 = [1,-1]
geraSequencia e = concatena (geraSequencia (e-1)) ([e,-e])  

-- 18.1) Recebe uma lista e retorna outra, que contém os mesmos elementos da primeira, em ordem invertida
inverte :: [t] -> [t]
inverte (c:r) = concatena (inverte r) [c]
inverte [] = []

-- 18.2) Recebe uma lista e retorna outra, que contém os mesmos elementos da primeira, em ordem invertida
inverte2 :: [t] -> [t]
inverte2 l = inverte' l []
    where
    inverte' [] l = l
    inverte' (c:r) l = inverte' r (c:l)


-- 20) Recebe duas listas e retorna outra lista com os elementos das listas originais intercalados
intercala :: [t] -> [t] -> [t]
intercala l [] = l
intercala [] n = n
intercala (c:r) (b:x) = c : b : (intercala r x)


-- 21) Recebe duas listas que não contenham elementos repetidos e retorna uma nova
--     com todos os elementos das duas listas originais (sem repetição)
uniao :: (Eq t) => [t] -> [t] -> [t]
uniao l n = removerRepetidos (l ++ n)


-- 22) Recebe duas listas sem elementos repetidos e retorna uma lista com os elementos que são comuns às duas
interseccao :: (Eq t) => [t] -> [t] -> [t]
interseccao (c:r) l
    | pertence c l  = c : (interseccao r l)
    | otherwise = interseccao r l
interseccao [] _ = []

-- 23) Recebe duas listas e verifica se elas tem os mesmos elementos
mesmos_elementos :: (Eq t) => [t] -> [t] -> Bool
mesmos_elementos [] [] = True
mesmos_elementos [] _ = False
mesmos_elementos (c:r) l
    | pertence c l = mesmos_elementos r (remove c l)
    | otherwise = False


-- 24) Recebe dois números naturais n e m, e retorna uma lista com n elementos, onde o primeiro é m, o segundo é m+1, etc...
sequencia :: Int -> Int -> [Int]
sequencia 0 _ = []
sequencia n m = m : (sequencia (n-1) (m+1))

-- 25)
insereOrdenado :: (Ord t) => [t] -> t -> [t]
insereOrdenado (c:r) e 
    | e < c = e : c : r
    | otherwise = c : (insereOrdenado r e)
insereOrdenado [] e = [e]

-- 26) 
verificaOrdem :: (Ord t) => [t] -> Bool
verificaOrdem [_] = True
verificaOrdem (c:r)
    | c <= (primeiroElemento r) = verificaOrdem r
    | otherwise = False
verificaOrdem [] = True

primeiroElemento :: [t] -> t
primeiroElemento (c:r) = c

-- Ou

verificaOrdem2 :: (Ord t) => [t] -> Bool
verificaOrdem2 [_] = True
verificaOrdem2 (c:a:r)
    | c <= a = verificaOrdem [a:r]
    | otherwise = False
verificaOrdem2 [] = True

algumaOrdem :: (Ord t) => [t] -> Bool
algumaOrdem l = (verificaOrdem l) || (verificaOrdem (inverte l))


-- 27) 
ordena :: (Ord t) => [t] -> [t]
ordena [] = []
ordena (c:r) = insereOrdenado (ordena r) c

quicksort :: (Ord t) => [t] -> [t]
quicksort (c:r) = (quicksort [x | x <- r, x <= c]) ++ [c]
                    ++ (quicksort [y | y <- r, y > c])
quicksort [] = []


--28)
picos ::  (Ord t) => [t] -> [t]
picos [e] = []
picos (c:r) = picos' (((last r):c:r) ++ [c])
    where
    picos' (a:b:c:r) 
        | (a < b) && (b > c) = b : picos' (c:r)
        | otherwise = picos' (b:c:r)
    picos' _ = []
picos [] = []


ultimoElemento :: [t] -> t
ultimoElemento [e] = e
ultimoElemento (c:r) = ultimoElemento r

--29)
rodarEsquerda :: Int -> [t] -> [t]
rodarEsquerda n (c:r)
    | n > 0 = rodarEsquerda (n-1) (r ++ [c])
    | otherwise = c:r

rodarDireita :: Int -> [t] -> [t]
rodarDireita n (c:r)
    | n > 0 = rodarDireita (n-1) ([(ultimoElemento r)]++[c]++(removerUltimo r))
    | otherwise = c:r

-- A programação funcional é um paradigma em que a
-- programação é uma declaração da resposta de um problema ao invés da declaração dos necessários para que
-- o computador chegue à resposta.

-- Diferença da programação funcional para as demais:
-- 1) Não há sequência de comandos, passa a ser uma descrição atemporal;
-- 2) Sendo atemporal, não faz sentido querer alterar o valor das coisas e as cópias de valores
-- são sempre implícitas (por exemplo pela passagem de parâmetros);
-- 3) Sendo atemporal, não faz sentido querer repetir coisas, a repetição é substituída pela recursão.
-- 4 )Reservar, usar e liberar espaços deixam de ser preocupações do programador para ser preocupações do compilador 
-- ou interpretador.


-- Vantagens e Desvantagens da programação Funcional :
-- A programação funcional tende a ser mais simples em termos de escrita, mais fácil de ler, mais fácil de
-- manter e menos eficiente do que a programação imperativa. Isso acontece por causa de redução de detalhes
-- (maior abstração) que são necessários na programação.


-- Inferência de tipos:
-- A inferência de tipos é um recurso que permite que o programador não precise declarar os tipos das
-- variáveis. O compilador resolve os tipos em tempo de compilação. 


-- Avaliação Preguiçosa:
-- A maioria das linguagens imperativas ou híbridas possui avaliação gulosa. Isso significa que o valor dos
-- operandos (incluindo parâmetros) é resolvido antes do processamento das operações.
-- Uma linguagem com avaliação preguiçosa (como as linguagens funcionais) deixa para avaliar os operandos tão tarde
-- quanto possível, na esperança de economizar processamento.

-- Recursão com cauda:
-- Em algumas recursões, o resultado da chamada recursiva não precisa ser processado para produzir
-- o resultado, ou seja, o resultado final é o resultado da chamada recursiva. Neste caso, o compilador ou
-- interpretador não precisaria criar variáveis para cada nova chamada e poderia gerar código de repetição que é
-- equivalente ao código dos laços.
-- Os compiladores ou interpretadores da programação funcional
-- são projetados para reconhecer recursão de cauda e gerar código equivalente ao dos laços.
-- Uma recursão comum pode ser transformada numa recursão de cauda pela criação de parâmetros auxiliares
-- que permitem que processamento possa ser feito sempre avançando (sem nunca precisar voltar) para
-- terminar um processamento.

-- Funções de Alta Ordem:
-- As funções de alta ordem (também chamadas de funções de ordem superior) recebem outras funções como
-- parâmetros ou retornam um resultado que é uma função. Elas não são usadas para um processamento
-- específico mas sim como formas de categorizar processamento (expressar modelos de processamento).

-- Funções lambda:
-- As funções lambda, provenientes do cálculo lambda, são descrições de associação de mapeamento que
-- representam processamento. São funções anônimas (sem declaração de nome) que aparecem em lugares
-- onde a sintaxe da linguagem espera uma expressão.

