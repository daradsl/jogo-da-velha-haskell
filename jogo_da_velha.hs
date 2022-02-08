--                      Trabalho 2
-- Jogo da Velha
-- Dara dos Santos Lima                              16103611



module Main where


import Data.Char
import System.IO
import Control.Exception
import System.IO.Error
import System.Process
import Data.List
import Data.Function


-- definicao do tipo jogador, formado por um nome e uma pontuacao
data Jogador = Pessoa String Int
 deriving(Eq,Show)

pontuacao :: Jogador -> Int
pontuacao (Pessoa n i) = i

nome :: Jogador -> String
nome (Pessoa n i) = n 


-- gera um tabuleiro vazio
geraNovoTabuleiro :: [[Char]]
geraNovoTabuleiro = [[' ', ' ', ' '],[' ', ' ', ' '],[' ', ' ', ' ']]


type GBoard = [[Char]]

gBoard :: GBoard
gBoard = geraNovoTabuleiro


-- gArr (get array):
gArr :: Int -> [t] -> t
gArr n []     = error "ERRO EXCEDEU O VETOR"
gArr 0 (x:xs) = x
gArr n (x:xs) = gArr (n-1) xs


-- uArr (update array):
uArr :: Int -> a -> [a] -> [a]
uArr p v []     = []
uArr 0 v (x:xs) = v : xs
uArr p v (x:xs) = x : uArr (p-1) v xs


-- gPos (get position):
gPos :: Int -> Int -> [[a]] -> a
gPos l c x = (gArr c (gArr l x))


-- uPos (update position):
uPos :: Int -> Int -> a -> [[a]] -> [[a]]
uPos l c v (x:xs)
    | (l==0)        = (uArr c v x) : xs
    | otherwise     = x : uPos (l-1) c v xs


-- Printa o tabuleiro com as opcoes de posicoes para jogar
printPosic :: String
printPosic  =  ("  0  " ++ "|" ++ "  1  " ++ "|" ++ "  2  " ++ "\n" ++ "_____" ++ "|" ++ "_____" ++ "|" ++ "_____" ++ "\n" ++"  3  " ++ "|" ++ "  4  " ++ "|" ++ "  5  " ++ "\n" ++"_____" ++ "|" ++ "_____" ++ "|" ++ "_____" ++ "\n" ++"  6  " ++ "|" ++ "  7  " ++ "|" ++ "  8  " ++ "\n" ++ "     " ++ "|" ++ "     " ++ "|" ++ "     " ++ "\n")


-- Printa os tabuleiros durante as jogadas
printBoard :: [[Char]] -> String
printBoard t = printando 0
        where 
            printando :: Int -> String
            printando 3 = last
            printando l = "  " ++ gPos l 0 t : "  " ++ "|" ++ "  " ++ gPos l 1 t : "  " ++ "|" ++ "  " ++ gPos l 2 t : "  " ++ linhas l ++ printando (l+1)
          
            linhas :: Int -> [Char]     -- entra com uma flag para printar o linhas apenas duas vezes (0 e 1)
            linhas 0 = "\n" ++ "_____" ++ "|" ++ "_____" ++ "|" ++ "_____" ++ "\n"
            linhas 1 = "\n" ++ "_____" ++ "|" ++ "_____" ++ "|" ++ "_____" ++ "\n"
            linhas _ = ""            

            last :: [Char]
            last = "\n" ++ "     " ++ "|" ++ "     " ++ "|" ++ "     " ++ "\n"


-- Funcao que retorna a linha e coluna de uma posicao do tabuleiro
pos :: Int -> (Int, Int)
pos 0 = (0, 0)
pos 1 = (0, 1)
pos 2 = (0, 2)
pos 3 = (1, 0)
pos 4 = (1, 1)
pos 5 = (1, 2)
pos 6 = (2, 0)
pos 7 = (2, 1)
pos 8 = (2, 2)
pos _ = error "Posicao Invalida "


-- Funcao que insere O ou X na pos desejada
abreJogada :: Int -> Int -> [[Char]] -> [[Char]]                       -- j me indica qual o jogador, se 1 ou 2  
abreJogada n j t = (jogada (fst (pos n)) (snd (pos n)) j t)                                         
    where
        jogada :: Int -> Int -> Int -> [[Char]] -> [[Char]]
        jogada l c j t
            | (gPos l c t == 'X') || (gPos l c t == 'O')         = error "Posicao Invalida"
            | (gPos l c t == ' ') && (j==1)                      = uPos l c 'X' t
            | (gPos l c t == ' ') && (j==2)                      = uPos l c 'O' t


-- Combinacoes possiveis de vitoria
vitorias :: [(Int, Int, Int)]
vitorias = [(0, 1, 2), (3, 4, 5), (6, 7, 8), (0, 3, 6), (1, 4, 7), (2, 5, 8), (0, 4, 8), (2, 4, 6)]


-- Funcoes que retornao o primeiro, segundo e terceiro elementos de triplas
pri :: (Int, Int, Int) -> Int
pri (a, b, c) = a

seg :: (Int, Int, Int) -> Int
seg (a, b, c) = b

terc :: (Int, Int, Int) -> Int
terc (a, b, c) = c


-- Verifica se alguem ganhou o jogo 
venceu :: [[Char]] -> Char -> Bool
venceu t c = verifica (vitorias)
        where
            verifica :: [(Int, Int, Int)] -> Bool
            verifica [] = False 
            verifica (x:xs)
                    | (((gPos (fst (pos (pri x))) (snd (pos (pri x))) t) == c) && ((gPos (fst (pos (seg x))) (snd (pos (seg x))) t) == c) && ((gPos (fst (pos (terc x))) (snd (pos (terc x))) t) == c))     = True
                    | otherwise                                                                                                                                                                             = verifica xs


-- Testa se empatou
-- se true empatou = deu velha
empate :: [[Char]] -> Bool
empate t = (percorre 0 t)

percorre :: Int -> [[Char]] -> Bool
percorre 3 t = True
percorre l t
        | (gPos l 0 t == ' ') || (gPos l 1 t == ' ') || (gPos l 2 t == ' ')      = False
        | otherwise                                                              = True && percorre (l+1) t


-- Exibe o menu de opcoes
menu :: IO()
menu = do 
        putStr "\n\n ------------ Jogo da Velha ------------ \n"
        putStr "\n Entre com a opcao desejada: "
        putStr "\n 1: Abrir novo jogo \n 0: Sair \n"


-- Executa a opcao desejada
opcao :: Char -> IO()
opcao '1' = do                        -- jogar
            getChar
            putStrLn " Entre com o nome do jogador 1: "
            s1 <- getLine
            let j1 = Pessoa s1 0      -- cria jogadores com pontuacao 0
            putStrLn " Entre com o nome do jogador 2: "
            s2 <- getLine
            let j2 = Pessoa s2 0
            putStrLn " ------ Posicoes ------- "
            putStr (printPosic)
            gameLoop (geraNovoTabuleiro) (nome j1) (nome j2) (pontuacao j1) (pontuacao j2)
opcao '0' = do                         -- sair
            putStr "\n Saindo..."
            return()
opcao _ = do                           -- opcoes invalidas
            putStr "\n Opcao Invalida, tente novamente "
            putStr "\n Digite enter para voltar "
            getChar
            getChar
            main


-- Mostra a pontuacao dos jogadores 
mostrarPontuacao :: Jogador -> Jogador -> IO()
mostrarPontuacao j1 j2 = do  
                    putStrLn "---------- Pontuacao ----------"
                    putStrLn (" Jogador 1: " ++ (nome j1) ++ " : " ++ show (pontuacao j1) ++ " pontos ")        
                    putStrLn (" Jogador 2: " ++ (nome j2) ++ " : " ++ show (pontuacao j2) ++ " pontos ")


-- iniciando vai ser a funcao que sempre que acaba uma rodada, vem pra ca e ve se comeca outra ou nao
-- sempre vai mostrar a pontuacao das rodadas ja jogadas 
-- ve se encerra ou joga novamente
iniciando :: Jogador -> Jogador -> IO()
iniciando j1 j2 = do 
            mostrarPontuacao j1 j2                         -- funcao que printa a pontuacao 
            putStrLn "\n Desejam jogar mais uma rodada? Digite 1 para Sim e 2 para Nao "
            op <- getChar
            getChar
            if (op == '1')
                then 
                    gameLoop (geraNovoTabuleiro) (nome j1) (nome j2) (pontuacao j1) (pontuacao j2) -- outro jogo
            else do 
                if (op == '2')
                    then do 
                            grandeVencedor j1 j2                   -- printa o vencedor e sai
                            putStr "\n Saindo..."
                            return()
                else do
                    putStr "\n Opcao invalida \n"
                    grandeVencedor j1 j2 
                    putStr "\n Saindo..."
                    return()


-- grandeVencedor diz quem eh o vencedor de todas as rodadas jogadas,
-- com tantos pontos
grandeVencedor :: Jogador -> Jogador -> IO()
grandeVencedor j1 j2 = do
                        if (pontuacao j1 > pontuacao j2)
                            then do 
                                putStr ("\n Jogador: " ++ nome j1 ++ "  é o vencedor!!!      com: " ++ show (pontuacao j1) ++ " pontos! \n")
                        else do 
                            if (pontuacao j1 == pontuacao j2)
                                then do 
                                    putStr "\n  Empate no total de rodadas!!!! "
                            else do 
                                putStr("\n Jogador: " ++ nome j2 ++ "  é o vencedor!!!      com: " ++ show (pontuacao j2) ++ " pontos! \n")


-- Funcao usada para incrementar a pontuacao dos jogadores
add :: Int -> Int
add 0 = 1
add n = n + 1


-- Funcao recursiva de um jogo inteiro
gameLoop :: [[Char]] -> String -> String -> Int -> Int -> IO()
gameLoop mb n1 n2 p1 p2 = do
                putStr (n1 ++ "\nEscolha uma posicao: ")
                j1 <- getLine
                let newGB = (abreJogada  (read j1) (1) mb)
                if (venceu newGB 'X')
                    then do 
                        putStr ("\n   VITORIA!   \n" ++ (n1) ++ " Venceu!!!! \n\n")
                        putStr $ printBoard newGB 
                        let j1New = Pessoa (n1) (add (p1))        -- atualiza a pontuacao 
                        iniciando (j1New) (Pessoa n2 p2)
                else do  
                    if (empate newGB)
                        then do  
                            putStr "\n\n  Deu Velha!!! Ninguem Venceu! \n\n"
                            putStr $ printBoard newGB 
                            iniciando (Pessoa n1 p1) (Pessoa n2 p2) 
                    else do
                        putStr $ printBoard newGB
                        putStr (n2 ++ "\nEscolha uma posicao: ")
                        j2 <- getLine
                        let newGB2 = (abreJogada (read j2) (2) newGB)
                        putStr $ printBoard (newGB2)
                        if (venceu newGB2 'O')
                            then do 
                                putStr ("\n   VITORIA!   \n" ++ (n2) ++ " Venceu!!!! \n\n")  
                                let j2New = Pessoa (n2) (add (p2))        -- atualiza a pontuacao 
                                putStr $ printBoard newGB2
                                iniciando (Pessoa n1 p1) j2New
                            else do  
                                if (empate newGB2)
                                    then do  
                                        putStr "\n\n  Deu Velha!!! Ninguem Venceu! \n\n"
                                        putStr $ printBoard newGB2
                                        iniciando (Pessoa n1 p1) (Pessoa n2 p2)  
                                        else
                                            gameLoop (newGB2) n1 n2 p1 p2 


-- Funcao main 
-- Mostra o menu e recebe e opcao desejada
main :: IO ()
main = do
   menu
   op <- getChar
   opcao op  
