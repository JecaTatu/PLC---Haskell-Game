{-# OPTIONS_GHC -Wall #-}

import System.IO
import Data.List
import System.Timeout
import System.Random
import Control.Applicative
import Control.Monad.Loops
import System.Console.ANSI
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async

-- Tipo representando uma tupla de inteiros
type Vector = (Int, Int)
-- MVAR que controla os valores do placar
type Score = MVar (Int, Int)
-- MVar representando o level que é incrementado a cada começo de partida
type Level = MVar Int
-- Tipo representando as coordenadas de movimento, em forma de uma tripla de inteiros
type MoveVector = (Int, Int, Int)

data State = State {
    -- Tamanho da tela
    board :: Int,
    -- Um array com as posições de cada player
    player1 :: [Vector],
    player2 :: [Vector],
    -- Arrays com as posições de cada item extra na tela
    evil :: [Vector],
    good :: [Vector],
    surprise :: [Vector],
    -- Uma chave para gerar número randomicos
    std :: StdGen,
    -- A direção e o sentido dos players
    move1 :: Maybe MoveVector,
    move2 :: Maybe MoveVector,
    -- As pontuações de cada jogador
    points1 :: Int,
    points2 :: Int,
    level :: Int,
    count :: Int
} deriving Show

-- Inicializando o jogo
main :: IO ()
main = do
        putStrLn "Vamos ao jogo, digite 1 para iniciar!"
        checker <- getLine
        fim <- newMVar 1
        score <- newMVar (0,0)
        level <- newMVar 1
        forkIO(newGame fim level score thematic)
        waitThreads fim
        return ()

waitThreads :: MVar Int -> IO ()
waitThreads fim = do 
    f <- takeMVar fim
    if (f > 0) then
        do putMVar fim f;
            waitThreads fim
    else
        return ()

-- Loop para vários jogos
-- Se vocễ digitar 'q' ele irá finalizar o jogo
-- Caso você digite qualquer outra tecla ele irá aumentar o level de dificuldade e começar de novo
newGame:: MVar Int -> Level -> Score -> [Char] -> IO ()
newGame fim level score checker = do
    game level score checker
    putStrLn "GGWP! Para jogar mais uma, mas com um level mais dificil, digite qualquer tecla, para sair digite q"
    resp <- getChar 
    if resp /= 'q' then do
        l <- takeMVar level
        putMVar level (l+1)
        newGame fim level score checker
        else do 
        putStrLn "Obrigado por jogar!";
        f <- takeMVar fim;
        putMVar fim (f-1);

-- A inicalização da tela de jogo
game :: Level -> Score -> [Char] -> IO State
game level score checker = clearScreen
    >> firstState score level
    >>= (iterateUntilM gameOver (step score checker))

-- Estado inicial de cada jogo, ele recebe o level e o score do jogo e retorna um estado
-- Este estado vai ter informações como posição dos players, do primeiro good, entre outras informações
-- Que são necessárias para o desenvolver do jogo

baseState :: Score -> Level -> IO State
baseState score level = do
                    (a,b) <- takeMVar score
                    putMVar score (a,b)
                    l <- takeMVar level
                    putMVar level l
                    stdGen <- getStdGen 
                    return State {
                board = 30, -- Tamanho da tela 
                player1 = [(4, 24)], --Posição do primeiro player
                player2 = [(49, 27)], -- Posição do segundo player
                std = stdGen,
                good = [randomPosition (concat (boardPositions 30)) stdGen], -- Cria um elemento bom em uma posição randomica em algum lugar da tela
                evil = [], -- No estado inicial não possui elementos maus
                surprise = [], -- No estado inicial não possui elementos surpresa
                move1  = Just (0, 1, 0),
                move2  = Just (1, -1, 0),
                points1 = a,
                points2 = b,
                level = l,
                count = 0
            }

-- Retorna um array de tuplas com todas as posições da tela;
boardPositions :: Int -> [[(Int, Int)]]
boardPositions size
    = [[(x, y) | x <- [0 .. (size*2 - 1)]] | y <- reverse [0 .. (size - 1)]]

-- Recebe um array e retorna uma posição aleatória do mesmo, é usado para
-- Geração aleatória de elementos tanto pra elementos positivos quanto para negativos
randomPosition :: [a] -> StdGen -> a
randomPosition xs inputStdGen = element
    where indexStdGenTuple = randomR (0, length xs - 1) inputStdGen
          element          = xs !! fst (indexStdGenTuple)

-- A cada movimento de um dos personagens o step chama a função updateState para atualizar todo o jogo
step :: Score -> [Char] -> State -> IO State
step score checker state = sample timer getInput 
    >>= \ inputMove ->
        displayState score checker $ updateState state (getVector inputMove) 

-- Espera um input e retorna o mesmo
getInput :: IO Char
getInput = hSetEcho stdin False 
    >> hSetBuffering stdin NoBuffering
    >> getChar

-- O tempo no qual os players se mexem, nesse caso, Um quarto de 1 segundo
timer :: Int
timer = div ((9 :: Int) ^ (6 :: Int)) 2

-- Thread que controla o tempo e os inputs
sample :: Int -> IO a -> IO (Maybe a)
sample n f = concurrently (timeout n f) (threadDelay n) 
            >>= \ (result, _) -> return result

-- Retorna uma tupla com 3 elementos que é retornada de acordo com o que o usuário digita
getVector :: Maybe Char -> Maybe MoveVector
getVector (Just 'w') = Just (0, 0,  1)
getVector (Just 'a') = Just (0, -1,  0)
getVector (Just 's') = Just (0, 0, -1)
getVector (Just 'd') = Just (0, 1,  0)
getVector (Just 'i') = Just (1, 0,  1)
getVector (Just 'j') = Just (1, -1,  0)
getVector (Just 'k') = Just (1, 0, -1)
getVector (Just 'l') = Just (1, 1,  0)
getVector _          = Nothing
