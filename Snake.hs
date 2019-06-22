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

-- Todos os updates são funções que mudam o estado dos players
updateState :: State -> Maybe MoveVector -> State
updateState state inputMove
        = updateItens $ updatePlayer1 $ updatePlayer2 $ updateMove state inputMove

updateMove :: State -> Maybe MoveVector -> State
updateMove state@(State { move1 = Just moveVector1, move2 = Just moveVector2}) inputMove@(Just inputVector)
    | first inputVector == 0 && inputVector /= moveVectorOpposite moveVector1 
        = state { move1 = inputMove <|> move1 state }
    | first inputVector == 1 && inputVector /= moveVectorOpposite moveVector2 
        = state { move2 = inputMove <|> move1 state }
updateMove state _ = state

updatePlayer1 :: State -> State
updatePlayer1 = updatePlayer1Tail . updateHeadPlayer1

updatePlayer2 :: State -> State
updatePlayer2 = updatePlayerTail2 . updateHeadPlayer2

--Aqui dá um update nos good e nos evil
updateItens :: State -> State
updateItens state@(State {points1 = s1, points2 = s2, good = g, evil = e, surprise = s, std = rd, level = l, count = c})
    | player1HasSurprise state = state { surprise = scrambleGoods (s), points1 = (s1+1),std = neo2}
    | player2HasSurprise state = state { surprise = scrambleGoods (s), points2 = (s2+1),std = neo2}
    | (player1HasBenefit state) && (c >= 4) = state { good = scrambleGoods (g ++ newGoods state index1), evil = e ++ newEvils state index2, surprise = s ++ newSurprises state index3, points1 = (s1+1),std = neo2}
    | (player2HasBenefit state) && (c >= 4) = state { good = scrambleGoods (g ++ newGoods state index1), evil = e ++ newEvils state index2, surprise = s ++ newSurprises state index3, points2 = (s2+1),std = neo2}
    | (player1HasBenefit state) && (c >= 2) = state { good = scrambleGoods (g ++ newGoods state index1), evil = e ++ newEvils state index2, points1 = (s1+1),std = neo2, count = (c+1)}
    | (player2HasBenefit state) && (c >= 2) = state { good = scrambleGoods(g ++ newGoods state index1), evil = e ++ newEvils state index2, points2 = (s2+1),std = neo2, count = (c+1)}
    | player1HasBenefit state = state { good = scrambleGoods (g ++ newGoods state index1), points1 = (s1+1),std = neo2, count = (c+1)}
    | player2HasBenefit state = state { good = scrambleGoods(g ++ newGoods state index1), points2 = (s2+1),std = neo2, count = (c+1)}
    | otherwise                  = state
    where indexStdGenTuple1 = randomR (0, 1) (rd)
          index1            = (fst indexStdGenTuple1)
          neo              = snd indexStdGenTuple1
          indexStdGenTuple2 = randomR (0, l) (neo)
          index2            = (fst indexStdGenTuple2)
          neo2              = snd indexStdGenTuple1
          indexStdGenTuple3 = randomR (1, 2) (neo2)
          index3            = (fst indexStdGenTuple3)

-- Adiciona um elemento na cabeça do vetor
updateHeadPlayer1 :: State -> State
updateHeadPlayer1 state@(State { move1 = (Just vector) })
    = state { player1 = head (player1 state) `vectorAdd` vector : player1 state }
updateHeadPlayer1 state = state

updateHeadPlayer2 :: State -> State
updateHeadPlayer2 state@(State { move2 = (Just vector) })
    = state { player2 = head (player2 state) `vectorAdd` vector : player2 state }
updateHeadPlayer2 state = state

-- Remove um elemento da cauda do vetor caso ele não tenha chegado a um beneficio ou uma surpresa
updatePlayer1Tail :: State -> State
updatePlayer1Tail state
    | player1HasBenefit state = state
    | player1HasSurprise state = rand1 state
    | otherwise                  = state { player1 = init $ player1 state }

updatePlayerTail2 :: State -> State
updatePlayerTail2 state
    | player2HasBenefit state = state
    | player2HasSurprise state = rand2 state
    | otherwise                  = state { player2 = init $ player2 state }

rand1 :: State -> State
rand1 state@(State {std = rd, points1 = s1})
    | (randomFunction [0,1] rd) == 0 = state { player1 = init $ (init $ (init $ (init $ player1 state))), points1 = (s1-2)}
    | (randomFunction [0,1] rd) == 1 = updateHeadPlayer1 ( updateHeadPlayer1 ( updateHeadPlayer1 state ))

rand2 :: State -> State
rand2 state@(State {std = rd, points2 = s2})
    | (randomFunction [0,1] rd) == 0 = state { player2 = init $ (init $ (init $ (init $ player2 state))), points2 = (s2-2)}
    | (randomFunction [0,1] rd) == 1 = updateHeadPlayer2 (updateHeadPlayer2 ( updateHeadPlayer2 state))
    
randomFunction :: [Int] -> StdGen -> Int
randomFunction x inputStdGen = element
    where indexStdGenTuple = randomR (0, length x - 1) inputStdGen
          element = fst (indexStdGenTuple)

-- Retorna se o player está com a cabeça em cima de um elemento bom
player1HasBenefit :: State -> Bool
player1HasBenefit state
    = head (player1 state) `elem` good state  

-- Retorna se o player está com a cabeça em cima de um elemento bom
player2HasBenefit :: State -> Bool
player2HasBenefit state
    = head (player2 state) `elem` good state  

-- Retorna se o player está com a cabeça em cima de um elemento surpresa
player1HasSurprise :: State -> Bool
player1HasSurprise state
    = head (player1 state) `elem` surprise state

-- Retorna se o player está com a cabeça em cima de um elemento surpresa
player2HasSurprise :: State -> Bool
player2HasSurprise state
    = head (player2 state) `elem` surprise state

vectorAdd :: Vector -> MoveVector -> Vector
vectorAdd (x1, y1) (a, x2, y2) = (x1 + x2, y1 + y2)

-- Retorna uma posição que não esteja ocupada por nenhum outro elemento
newGood :: State -> StdGen -> Vector
newGood state st
    = randomPosition validPositions5 st
        where allPositions   = concat $ boardPositions $ board state
              validPositions1 = allPositions \\ player1 state
              validPositions2 = validPositions1 \\ player2 state
              validPositions3 = validPositions2 \\ evil state
              validPositions4 = validPositions3 \\ good state
              validPositions5 = validPositions4 \\ surprise state

--Aqui gera pelo menos um elemento bom
newGoods :: State -> Int -> [Vector]
newGoods state 0 = [newGood state (mkStdGen 1)]
newGoods state n = [newGood state (mkStdGen (n+1))] ++ newEvils state (n-1)

--Aleatorizar os elementos bons
scrambleGoods :: [Vector] -> [Vector]
scrambleGoods g = map mix g

mix :: Vector -> Vector
mix (a,b) = ((a*a `mod` 60),(b*a `mod` 30))

-- Retorna uma posição que não esteja ocupada por nenhum outro elemento
newEvil :: State -> StdGen -> Vector
newEvil state st
    = randomPosition validPositions5 st
        where allPositions   = concat $ boardPositions $ board state
              validPositions1 = allPositions \\ player1 state
              validPositions2 = validPositions1 \\ player2 state
              validPositions3 = validPositions2 \\ evil state
              validPositions4 = validPositions3 \\ good state
              validPositions5 = validPositions4 \\ surprise state

newEvils :: State -> Int -> [Vector]
newEvils state 0 = [newEvil state (mkStdGen 0)]
newEvils state n = [newEvil state (mkStdGen n)] ++ newEvils state (n-1)

-- Retorna uma posição que não esteja ocupada por nenhum outro elemento
newSurprise :: State -> StdGen -> Vector
newSurprise state st
    = randomPosition validPositions5 st
        where allPositions   = concat $ boardPositions $ board state
              validPositions1 = allPositions \\ player1 state
              validPositions2 = validPositions1 \\ player2 state
              validPositions3 = validPositions2 \\ evil state
              validPositions4 = validPositions3 \\ good state
              validPositions5 = validPositions4 \\ surprise state

newSurprises :: State -> Int -> [Vector]
newSurprises state 0 = [newSurprise state (mkStdGen 0)]
newSurprises state n = [newSurprise state (mkStdGen n)] ++ newSurprises state (n-1) 


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