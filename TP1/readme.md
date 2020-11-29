# Mapello 

![Mapello](https://nestorgames.com/gameimages/mapello_mid.jpg)

## Turma 1 Mapello_5

### Trabalho realizado por:

| Nome          | Número    |
| --------------|-----------|
| Pedro Miguel Oliveira Azevedo | 201603816 |
| Diogo Ferreira de Sousa | 201706409 |

## Instalação e execução

Extrair os conteúdos do zip e usando o SICStus fazer a consulta do ficheiro game.pl .

Para executar o jogo é necessário executar o predicado play/0. Após executar o predicado pode escolher entre 2 opções :
- Player vs Player 
- Player vs CPU

Após selecionar Player vs Player é apenas necessário selecionar as posições do jokers e jogar normalmente. No caso de selecionar Player vs CPU temos de selecionar o lado do CPU, a sua dificuldade, as posições do joker e depois jogar o jogo normalmente.

## Descrição:

O mapello é uma variante do reversi no qual é adicionado algumas peças extras em comparação com o original, sendo elas as Walls (peças que servem como as paredes do tabuleiro e não podem ser colocadas peças em cima), Bonuses (peças que dão mais pontos a quem os captura) e Jokers (peças normais que pertencem ao jogador atual). O objetivo do jogo é terminar o jogo com o maior número de pontos (pontos = peças + pontos bonus) isto faz-se capturando peças inimigas, tornando as da cor do jogador.

### Regras

- Apenas pode colocar uma peça por turno
- No início do jogo, o tabuleiro começa com 4 peças no centro (Ex.: B W | W B), 8 jokers nas bordas do tabuleiro e 8 bonus e paredes no meio do tabuleiro.
- Só pode colocar a peça se consegue capturar peças inimigas (Ex.:B W W _)
- Se não conseguir colocar nenhuma peça passa a jogada para o próximo jogador.
- Se os 2 jogadores passarem consecutivos o jogo acaba e ganha quem tiver mais pontos.
- O jogo acaba quando não houver mais jogadas possíveis (Ex.: Tabuleiro completamente cheio).

### Links

-https://nestorgames.com/#mapello_detail
-https://cardgames.io/reversi/

## Representação de Interna do estado do Jogo

O estado do tabuleiro está guardado numa lista de listas (10 x 10) em qual cada posição vai guardar a peça que está aí. Em cada chamada para fazer a jogada passamos o número de bónus colecionados durante o jogo para cada jogador e adiciona esse bónus ao número de peças da mesma cor que cada jogador tem no tabuleiro. 

### Peças
As peças na lista de listas são representadas da seguinte forma:
- white : peças brancas
- black : peças pretas
- none : espaço vazio
- wall : parede
- joker : joker
- bonus : bonus


### Estado Inicial

No estado inicial temos um tabuleiro N * N gerado pela função na qual preenchemos todos os espaços da lista com 'none' , que em seguida vamos inicializar as posições das peças iniciais (2 peças brancas, 2 peças pretas, 8 paredes, 8 jokers, 8 bonus).

```prolog
% play
% Starts a new game
play:-
   initRandom,
   initial(B0),
   nl, write('Performing random Wall and Bonus pieces placement'), nl,
   wallSetupPhase(B0, 8, B1),
   bonusSetupPhase(B1, 8, B2),
   jokerSetupPhase(B2, 8, B3),
   playGame(B3, 0, 16, _).
 ```
#### Setting Random Walls and Bonus
![Walls and Bonus](https://github.com/Pedrag1234/FEUP_PLOG/blob/master/TP1/img/RandomwallsandBonus.PNG)

#### Setting Jokers 
![Jokers](https://github.com/Pedrag1234/FEUP_PLOG/blob/master/TP1/img/jokers.PNG)

### Estado Intermédio

O estado intermédio do jogo é quando ambos os jogadores conseguem fazer jogadas ou pelo menos um deles consegue jogar, nesta fase vamos adicionando peças brancas e pretas que por sua vez capturam peças dos inimigos tornado-as da mesma cor. No caso dos bonus


#### Play Loop
![Play](https://github.com/Pedrag1234/FEUP_PLOG/blob/master/TP1/img/play.PNG)

### Estado Final
O jogo termina quando uma das 2 condições é atingingida:
  - Não existem mais espaços vazios no tabuleiro.
  - Ambos os jogadores passam as sua vez.


## Visualização dos estados do jogo

A visualização do tabuleiro é feita chamando a função:

```prolog
% display_game(+Board, +Player)
% Displays the current game state, and announces next player turn

display_game(Board, Player):-
    printBoard(Board),
    write('Player '),
    write(Player),
    write(' Turn'), nl.
```
Esta função por sua vez chama a função printBoard que por sua vez vai linha a linha e depois elemento a elemento imprimir o conteúdo do tabuleiro. 

```prolog
% printBoard(+Board, )
% Displays the current board state
printBoard(Board):-
    nl,
    printHeader,
    printRows(Board, 10), nl.
    
% printRows(+Board, +N)
% Displays all the rows on the given board
printRows(_, 0).
printRows([FirstElem|OtherElem], N):-
    Number is 10 - N,
    printRowNumber(Number),
    N1 is N-1,
    printRow(FirstElem),
    printRowSep,
    printRows(OtherElem,N1).
    
% printRow(+Board)
% Displays a single row of the given board
printRow([]):-
    nl.
printRow([FirstElem|OtherElem]):-
    getRep(FirstElem,Label),
    printRep(Label),
    printColumnSep,
    printRow(OtherElem).
```

Cada peça é representada no tabuleiro da seguinte forma:
- 'W' : peças brancas
- 'B' : peças pretas
- '.' : espaço vazio
- 'G' : parede
- 'J' : joker
- 'P' : bonus

### Interações do Utilizador

O utilizado nos vários momentos de utilização é pedido a leitura de vários inputs do utilizador. Os inputs feitos infelizmente não são controlados de qualquer forma, logo inputs incorretos poderão causar problemas na execução do programa.

As várias interações que os utilizadores podem realizar são:
- Escolher o modo jogo (selecionar entre 1 a 2).
- Escolher as posições do Jokers (selecionar 8 coordenadas em que o X e o Y estão entre 0 e 9).
- Escolher a posição da peça a jogar ( 1 coordenada em que o X e o Y estão entre 0 e 9).

//TODO: add Fotos

## Lista de Jogadas Válidas

A lista de todas as jogadas válidas é calculada usando a função valid_moves(+BoardState, +Player, -ListOfMoves) que por sua vez retorna todas as posições das jogadas válidas ou um array vazio.

```prolog
% canPlay(+Board, +Player)
% checks if the player can make any plays    
canPlay(Board,Player,Plays):-
    checkAllValidMoves(Board,Player,Points,0,0),
    length(Points,N),
    N1 is N - 1,
    (compare(=,N1,0) -> fail),
    Plays is Points.
```

Para verificar quais a jogadas possíveis o predicado chama o predicado validate_play(+Board,+X,+Y,+Player) (que retorna se o Player consegue jogar na posição X,Y) para todas as posições vazias do tabuleiro do jogo. 


```prolog
% checkAllValidMoves(+Board, +Player, -Table, +Y, +X)
% returns an array with all possible plays   
checkAllValidMoves(_,_,_,10,_).
checkAllValidMoves(Board, Player, [H|T], Y, X):-
    (validatePlay(Board,X,Y,Player) -> H = [X,Y],MOVE is 0 ; MOVE is 1),
    (compare(=,X,9) -> X1 is 0, Y1 is Y + 1; X1 is X + 1, Y1 is Y),
    (compare(=,MOVE,0) -> checkAllValidMoves(Board, Player, T, Y1, X1) ; (compare(=,Y1,10) -> checkAllValidMoves(Board, Player, T, Y1, X1); checkAllValidMoves(Board, Player, [H|T], Y1, X1) )).
```

## Execução de Jogadas

A execução de jogadas tem várias funções dependendo do modo de jogo. 
No caso de Player vs Player é chamado o predicado makePlayerTurn(+Board, +Player, -NewBoard).

```prolog
% makePlayerTurn(+Board, +Player, -NewBoard)
% Goes through a player's turn on the game
makePlayerTurn(Board, 0, NewBoard):-
    placeDiscPlayer1(Board, NewBoard).

makePlayerTurn(Board, 1, NewBoard):-
    placeDiscPlayer2(Board, NewBoard).
```

No caso de Player vs CPU é chamad o predicado makeCPUTurn(+Board, +Player, +Difficulty -NewBoard).

```prolog
% makeCPUTurn(+Board, +Player, +Difficulty -NewBoard)
% Goes through a CPU's turn on the game
makeCPUTurn(Board, 0, Difficulty, NewBoard):-
    placeDiscCPU1(Board, Difficulty, NewBoard).

makeCPUTurn(Board, 1, Difficulty, NewBoard):-
    placeDiscCPU2(Board, Difficulty, NewBoard).
```

## Final do Jogo

A verificação do estado final do jogo é feita usando a função 
