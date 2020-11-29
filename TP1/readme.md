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

A validação das jogadas é feita usando a função validatePlay(+Board,+X,+Y,+Player) que por sua vez verifica em todas as direções se existe outra peça (joker ou peça da mesma cor) de forma a que consiga capturar pelo menos uma peça inimiga.

```prolog
% validatePlay(+Board,+X,+Y,+Player)
% checks if play made by the player is valid
validatePlay(Board,X,Y,Player):-
    checkInput('Disc', X, Y),
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, black),
    \+compare(=, Piece, white),
    \+compare(=, Piece, wall),
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    (checkLeft(Board,X1,Y, Player,0);
    checkRight(Board,X2,Y, Player,0);
    checkUp(Board,X,Y1, Player,0);
    checkDown(Board,X,Y2, Player,0);
    checkLeftUp(Board,X1,Y1, Player,0);
    checkLeftDown(Board,X1,Y2, Player,0);
    checkRightUp(Board,X2,Y1, Player,0);
    checkRightDown(Board,X2,Y2, Player,0)).
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

No caso de Player vs CPU é chamada o predicado makeCPUTurn(+Board, +Player, +Difficulty -NewBoard).

```prolog
% makeCPUTurn(+Board, +Player, +Difficulty -NewBoard)
% Goes through a CPU's turn on the game
makeCPUTurn(Board, 0, Difficulty, NewBoard):-
    placeDiscCPU1(Board, Difficulty, NewBoard).

makeCPUTurn(Board, 1, Difficulty, NewBoard):-
    placeDiscCPU2(Board, Difficulty, NewBoard).
```

## Final do Jogo

A verificação do estado final do jogo é feita usando a função game_over(+Board-,Winner,+Skips), que por sua vez retorna os jogador com o maior score.

```prolog
% game_over(+Board-,Winner,+Skips)
% returns winner of the game
game_over(Board, Winner, Skips):-
    isGameOver(Board,0,0,Skips),
    getBlackPlayerScore(Board, N1),
    getWhitePlayerScore(Board,N2),
    (N1 > N2 -> Winner is black ; Winner is white).
```

## Avaliação do Tabuleiro

A avaliação do tabuleiro é feita usando o número de peças do player presentes, isto é quanto mais peças presentes no tabuleiro melhor será o score. Esta avaliação é feita usando a

```prolog
% getBlackPlayerScore(+Board,-N)
% checks there is a cell empty in the row
getBlackPlayerScore(Board,N):-
    getAllBlackRowsScores(Board,0,0,Scores),
    sumlist(Scores,CombScores),
    N = CombScores.

% getWhitePlayerScore(+Board,-N)
% checks there is a cell empty in the row
getWhitePlayerScore(Board,N):-
    getAllWhiteRowsScores(Board,0,0,Scores),
    sumlist(Scores,CombScores),
    N = CombScores.
```

## Jogada do Computador

O computador para fazer a sua jogada usa a função makeCPUTurn(+Board, +Player, +Difficulty -NewBoard) na qual tem comportamentos diferentes dependendo da dificuldade escolhida. 

```prolog
% makeCPUTurn(+Board, +Player, +Difficulty -NewBoard)
% Goes through a CPU's turn on the game
makeCPUTurn(Board, 0, Difficulty, NewBoard):-
    sleep(2),
    placeDiscCPU1(Board, Difficulty, NewBoard).

makeCPUTurn(Board, 1, Difficulty, NewBoard):-
    sleep(2),
    placeDiscCPU2(Board, Difficulty, NewBoard).
```

No caso de dificuldade ser fácil, o AI escolhe de todas as jogadas possíveis uma aleatória usando as funções placeDiscCPU1(+Board, +Difficulty, -NewBoard) e placeDiscCPU2(+Board, +Difficulty, -NewBoard). 

```prolog
% placeDiscCPU1(+Board, +Difficulty, -NewBoard)
% Places a black Disc (owned by a CPU player 1) on the board
placeDiscCPU1(Board, 1, NewBoard):-
    canPlay(Board, black, ValidMoves),
    length(ValidMoves, MovesAmount),
    random(1, MovesAmount, MoveNumber),
    getMove(ValidMoves, MoveNumber, Move),
    Move = [X,Y],
    setPiece(Board, X, Y, black, TempBoard),
    capturePieces(TempBoard, black, white, X, Y, NewBoard).

placeDiscCPU1(Board, 2, NewBoard):-
    !.

% placeDiscCPU2(+Board, +Difficulty, -NewBoard)
% Places a white Disc (owned by a CPU player 2) on the board
placeDiscCPU2(Board, 1, NewBoard):-
    canPlay(Board, white, ValidMoves),
    length(ValidMoves, MovesAmount),
    random(1, MovesAmount, MoveNumber),
    getMove(ValidMoves, MoveNumber, Move),
    Move = [X,Y],
    setPiece(Board, X, Y, white, TempBoard),
    capturePieces(TempBoard, white, black, X, Y, NewBoard).

placeDiscCPU2(Board, 2, NewBoard):-
    !.
```

As jogadas aleatórias são obtidas usando a usando a funçao canPlay(+Board, +Player, -ValidMoves) que retorna uma lista de coordenadas de todas as jogadas que possam ser feitas.

```prolog
% canPlay(+Board, +Player, -Points)
% checks if the player can make any plays    
canPlay(Board,Player,Points):-
    checkAllValidMoves(Board,Player,Points,0,0),
    length(Points,N),
    N1 is N - 1, !,
    N1 =\= 0.
    
% checkAllValidMoves(+Board, +Player, -Table, +Y, +X)
% returns an array with all possible plays
checkAllValidMoves(_,_,[],10,_).
checkAllValidMoves(Board, Player, [H|T], Y, X):-
    (validatePlay(Board,X,Y,Player) -> H = [X,Y],MOVE is 0 ; MOVE is 1),
    (compare(=,X,9) -> X1 is 0, Y1 is Y + 1; X1 is X + 1, Y1 is Y),
    (compare(=,MOVE,0) -> checkAllValidMoves(Board, Player, T, Y1, X1) ; (compare(=,Y1,10) -> checkAllValidMoves(Board, Player, T, Y1, X1); checkAllValidMoves(Board, Player, [H|T], Y1, X1) )).

% validatePlay(+Board,+X,+Y,+Player)
% checks if play made by the player is valid
validatePlay(Board,X,Y,Player):-
    checkInput('Disc', X, Y),
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, black),
    \+compare(=, Piece, white),
    \+compare(=, Piece, wall),
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    (checkLeft(Board,X1,Y, Player,0);
    checkRight(Board,X2,Y, Player,0);
    checkUp(Board,X,Y1, Player,0);
    checkDown(Board,X,Y2, Player,0);
    checkLeftUp(Board,X1,Y1, Player,0);
    checkLeftDown(Board,X1,Y2, Player,0);
    checkRightUp(Board,X2,Y1, Player,0);
    checkRightDown(Board,X2,Y2, Player,0)).
```

## Conclusões:

Devido a limitações de tempo e alguns erros um bocado limitantes não fomos capazes de implementar tudo de forma funcional e sem erros. Alguns dos erros que foram encontrados ao longo do desenvolvimento do projeto:
   - O número de peças retornado pelas funções de avaliação do score em alguns casos são valores errados devido ao facto de não contar corretamente o número de peças (ao comparamos a peça numa posição com a peça da cor do jogador apesar de ser verdade retorna falso).
   - Uma situação semelhante ocorre com a inicialização das peças de bónus e as paredes que ao selecionarem uma posição aleatória se a posição não estiver vazia inserem na mesma essa peça.

Estes erros seriam os importantes de corrigir uma vez que impedem o funcionamento normal do jogo. Também implementar alguma maneira de controlar o input dos utilizadores para impedir erros causados pelos mesmos (Ex.: X = a, casusa terminação).É de adicionar que o AI está demasiado simplista neste momento e que talvez se conseguissemos aumentar a sua complexidade através de um algoritmo Alfa-Beta Cut com várias heurísticas para avaliação do tabuleiro tornaria os jogos de CPU vs CPU e Player vs CPU mais interessantes e mais desafiantes.


## Bibliografia

- Slides das teóricas
- Manual do SICStus
- https://nestorgames.com/#mapello_detail
- https://cardgames.io/reversi/
   
   
