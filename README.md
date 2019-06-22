# PLC---Haskell-Game


### Projeto para a Disciplina de PLC - 2019.1

Primeiro de tudo, se você não tem haskell e cabal instalado, instale!
$ sudo apt-get install haskell-platform

$ sudo apt-get install cabal-install

$ cabal update

$ cabal install mtl

$ sudo apt-get install libghc-ghc-mtl-dev

Após o cabal estar instalado, execute esse comando na pasta do projeto para instalar as dependências:

$ cabal build

Depois execute:
$ ghc -threaded -o game Puzzle.hs

E por fim:
$ ./game

