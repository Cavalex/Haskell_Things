{- |

= Introdução

O objetivo desta tarefa passou por implementar uma função que compactasse um qualquer "Maze" numa lista de instruções. 
Esta não nos apresentou tanta dificuldade na sua realização como noutras, sendo que a dividimos em mais partes visando
o resultado final.


= Objetivos

Como o objetivo final da tarefa passava por compactar um "Maze", decidimos a princípio compactar os corredores, assim
seguindo as diretrizes conseguimos ter todo um corresdor condensado a um conjunto de instruções.
Precisariamos agora de algo que juntasse todos os corredores, e foi daí que surgiu uma nova função que juntaria todos os corredores.
Com esta parte feita precisavamos de aplicar os repeats a linhas repetidas dentro do "Maze".
Passamos então à função que criaria os repeats, para esta ser mais eficaz e mais simples de fazer criamos uma elemIndices que calcula 
a lista de posições em que um dado elemento ocorre numa lista.
Agora, finalmente precisavamos de uma que condensasse e aplica-se os repeats, assim sendo, juntamos a função que faria do "Maze" 
uma lista de instruções e a que aplicaria os repeats e obtivemos o resultado final.


= Discussão e Conclusão

Tudo correu como previsto e o 'compactMaze' era capaz de condensar os labirintos de teste e passou a todos os testes que foi submetido.


-}

module Tarefa3 where
    
import Types
import Tarefa1

-- * Função principal 


-- | Para fazer esta função mais facilmente dividimo-la em duas funções, a 'initialCompact' e a 'applyRepeats'.
--   Posteriormente juntamo-la aqui para funcionar tudo corretamente

compactMaze :: Maze         -- ^ O 'Maze' semi-aleatório recebido
            -> Instructions -- ^ As 'Instructions' resultantes daquele 'Maze' 
compactMaze [] = []
compactMaze m = applyRepeats $ initialCompact m


-- * Funções auxiliares

-- | Criámos a 'initialCompact' com o intuito de especificar como passar o 'Maze' para 'Instructions' sem ter em conta os __Repeats__.
--   Usamos a 'compactCorridor' para transformar um 'Maze' em 'Instructions'
initialCompact :: Maze         -- ^ O 'Maze' recebido 
               -> Instructions -- ^ As 'Instructions' resultantes sem os Repeats
initialCompact [] = []
initialCompact (x:xs) = (Instruct (compactCorridor x)) : initialCompact xs

-- | Sendo [(Int, Piece)] um multi-conjunto de 'Pieces' então dado um 'Corridor' ela calcula o multi-conjunto dos seus elementos.

compactCorridor :: Corridor       -- ^ Um qualquer 'Corridor' de um certo Maze
                -> [(Int, Piece)] -- ^ O tuplo resultante da análise do 'Corridor'
compactCorridor [] = []
compactCorridor (x:xs) = aux xs x 1
    where
        aux [] elem num = [(num, elem)]
        aux (y:ys) elem num
            | y == elem = aux ys elem (num+1)
            | otherwise = (num, elem):aux ys y 1

-- | Chamamos à nossa função 'applyRepeats', esta por outro lado vai analisar as 'Instructions' e vai retornar outras 'Instructions' já com o __Repeat__ aplicado. 
--   Para o fazer esta nossa função usará duas funções auxiliares, uma que deteta 'Instruction' iguais e a outra que as substitui pelo __Repeat n __ correto.             

applyRepeats :: Instructions -- ^ As 'Instructions' sem os Repeats 
             -> Instructions -- ^ As 'Instructions' com os Repeats
applyRepeats m = aux m 0
    where
        aux [] _ = []
        aux (x:xs) ac
            | x `elem` xs && isXRepeat == False = x:aux (aux2 xs (ac) 0 (elemIndices x xs)) (ac+1)
            | otherwise = x:aux xs (ac+1)
            where
                isXRepeat = case x of Repeat _ -> True
                                      _ -> False
        -- aux2 (instruçoes) (numero do repeat) (acumulador) (linhas para mudar)
        aux2 [] _ _ _ = []
        aux2 is _ _ [] = is
        aux2 (x:xs) n ac (y:ys)
            | ac == y = (Repeat n):(aux2 xs n (ac+1) ys)
            | otherwise = x:(aux2 xs n (ac+1) (y:ys))


-- | Calcula a lista de posições em que um dado elemento ocorre numa lista.

elemIndices :: Eq a => a -- ^ Um "a" que seja comparável
            -> [a]       -- ^ Uma lista de a's
            -> [Int]     -- ^ Lista de posições em que o elemento passado acima ocorre na lista passada acima  
elemIndices n [] = []
elemIndices n l | n == last l = elemIndices n (init l) ++ [(length l-1)]
                 | otherwise = elemIndices n (init l)


