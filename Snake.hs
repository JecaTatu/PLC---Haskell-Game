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
        thematic <- getLine
        fim <- newMVar 1
        score <- newMVar (0,0)
        level <- newMVar 1
        forkIO(newPuzzle fim level score thematic)
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