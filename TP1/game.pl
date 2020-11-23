:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).
:- include('board.pl').

% initRandom
% Sets the random number generator with a time-based seed
initRandom:-
    now(N),
    Seed is ((N mod 30265) + 1),
    setrand(random(Seed, Seed, Seed, N)).

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

% display_game(+Board, +Player)
% Displays the current game state, and announces next player turn
display_game(Board, Player):-
    printBoard(Board),
    write('Player '),
    write(Player),
    write(' Turn'), nl.

% makeTurn(+Board, +Player, -NewBoard)
% Goes through a player's turn on the game
makeTurn(Board, 0, NewBoard):-
    placeDiscPlayer1(Board, NewBoard).

makeTurn(Board, 1, NewBoard):-
    placeDiscPlayer2(Board, NewBoard).

% playGame(+Board, +Turns, -NewBoard)
% Goes through each player's turn on the game
playGame(Board, _, 0, _):-
    printBoard(Board),
    write('Game Over!').
playGame(Board, Player, Turns, NewBoard):-
    NewTurns is Turns - 1,
    PlayerNum is Player mod 2,
    NewPlayer is Player + 1,
    display_game(Board, NewPlayer),
    makeTurn(Board, PlayerNum, TempBoard),
    playGame(TempBoard, NewPlayer, NewTurns, NewBoard).

% readInput(-Input)
% Reads a char input by the player    
readInput(Input):-
        get_char(Char),
        readEnter(Char, Input).

% readEnter(+Char, -[Char|Rest])
% Continues reading the stream, stopping when finding a newline
readEnter('\n', []).
readEnter(Char, [Char|Rest]) :-
        get_char(Char1),
        readEnter(Char1, Rest).

% readCoordinates(+PieceStr, -X, -Y)
% Reads coordinates input by the player, used to place a given piece
readCoordinates(PieceStr, X, Y):-
    write(PieceStr), write(' X Coordinate: '),
    readInput(Xinput),
    write(PieceStr), write(' Y Coordinate: '),
    readInput(Yinput),
    nth0(0, Xinput, Xchar), 
    nth0(0, Yinput, Ychar),
    number_chars(X, [Xchar]), number_chars(Y, [Ychar]).

% checkInput(+PieceStr, +X, +Y)
% Checks if the coordinates used are valid for the given piece
checkInput('Joker', X, Y) :-
    between(0, 9, X),
    (Y == 0; Y == 9).

checkInput('Joker', X, Y) :-
    between(0, 9, Y),
    (X == 0; X == 9).

checkInput('Disc', X, Y) :-
    between(0, 9, X),
    between(0, 9, Y).

% validateJokerInput(+Xinput, +Yinput)
% Checks if the coordinates used are valid for placing a Joker
validateJokerInput(Xinput,Yinput):-
    checkInput('Joker',Xinput,Yinput).

% validateDiscInput(+Xinput, +Yinput)
% Checks if the coordinates used are valid for placing a Disc
validateDiscInput(Xinput,Yinput):-
    checkInput('Disc',Xinput,Yinput).

% checkPlace(+Board, +X, +Y)
% Checks if there is already a piece placed at the given coordinates
checkPlace(Board, X, Y) :-
    getPiece(X, Y, Board, Piece),
    getRep(Piece,_,Value),
    if(Value == 1, true, fail).

% jokerSetupPhase(+Board, +N, -NewBoard)
% Sets up the initial Jokers on the board
jokerSetupPhase(Board, 0, Board).
jokerSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    printBoard(Board),
    placeJoker(Board, TempBoard),
    jokerSetupPhase(TempBoard, N1, NewBoard).

% wallSetupPhase(+Board, +N, -NewBoard)
% Sets up the initial Walls on the board     
wallSetupPhase(Board, 0, Board).
wallSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    (placeWall(Board, TempBoard) -> wallSetupPhase(TempBoard, N1, NewBoard) ; wallSetupPhase(Board, N, NewBoard)).

% bonusSetupPhase(+Board, +N, -NewBoard)
% Sets up the initial Bonuses on the board    
bonusSetupPhase(Board, 0, Board).
bonusSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    (placeBonus(Board, TempBoard) -> bonusSetupPhase(TempBoard, N1, NewBoard) ; bonusSetupPhase(Board, N, NewBoard)).

% placeDiscPlayer1(+Board, -NewBoard)
% Places a black Disc (owned by player 1) on the board
placeDiscPlayer1(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,black)) -> setPiece(Board,X,Y,black,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer1(Board,NewBoard)).

% placeDiscPlayer2(+Board, -NewBoard)
% Places a white Disc (owned by player 2) on the board
placeDiscPlayer2(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,white)) -> setPiece(Board,X,Y,white,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer2(Board,NewBoard)).

% placeJoker(+Board, -NewBoard)
% Places a Joker on the board
placeJoker(Board, NewBoard):-
    readCoordinates('Joker', X, Y),
    (validateJokerInput(X,Y) -> setPiece(Board,X,Y,joker,NewBoard) ; write('Jokers must be placed on the outer border, input again\n'), nl, placeJoker(Board,NewBoard)).

% placeWall(+Board, -NewBoard)
% Places a Wall on the board
placeWall(Board, NewBoard):-
     random(1, 9, X),
     random(1, 9, Y), !,
     checkPlace(Board,X,Y),
     setPiece(Board, X, Y, wall, NewBoard).

% placeBonus(+Board, -NewBoard)
% Places a Bonus on the board
placeBonus(Board, NewBoard):-
     random(1, 9, X),
     random(1, 9, Y),
     checkPlace(Board,X,Y),
     setPiece(Board, X, Y, bonus, NewBoard).
    
% initial
% Default initial board state
initial(X):-
    createEmptyBoard(B0),
    setInitialPieces(B0,B1),
    X = B1.


validatePlay(Board,X,Y,Player):-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    checkLHorizontal(Board,X1,Y,Player);
    checkRHorizontal(Board,X2,Y,Player);
    checkUVertical(Board,X,Y1,Player);
    checkDVertical(Board,X,Y2,Player);
    checkLUDiagonal(Board,X1,Y1,Player);
    checkLDDiagonal(Board,X1,Y2,Player);
    checkRUDiagonal(Board,X2,Y1,Player);
    checkRDDiagonal(Board,X2,Y2,Player).


% checkLHorizontal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces to the left of the pos
checkLHorizontal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkLHorizontal(Board,NX1,NY, Player))).

checkLHorizontal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkLHorizontal(Board,NX1,NY, Player))).


% checkRHorizontal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces to the right of the pos
checkRHorizontal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkRHorizontal(Board,NX1,NY, Player))).

checkRHorizontal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkRHorizontal(Board,NX1,NY, Player))).


% checkUVertical(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces above of the pos
checkUVertical(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NY >= 0,
    NY1 is NY - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkUVertical(Board,NX,NY1, Player))).

checkUVertical(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NY >= 0,
    NY1 is NY - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkUVertical(Board,NX,NY1, Player))).


% checkDVertical(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces bellow of the pos
checkDVertical(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NY =< 9,
    NY1 is NY + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkDVertical(Board,NX,NY1, Player))).

checkDVertical(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NY =< 9,
    NY1 is NY + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkDVertical(Board,NX,NY1, Player))).


% checkRUDiagonal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces diagonally to the right and up of the pos
checkRUDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NY >= 0,
    NY1 is NY - 1,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkRUDiagonal(Board,NX1,NY1, Player))).

checkRUDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NY >= 0,
    NY1 is NY - 1,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkRUDiagonal(Board,NX1,NY1, Player))).


% checkRDDiagonal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces diagonally to the right and down of the pos
checkRDDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NY =< 9,
    NY1 is NY + 1,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkRDDiagonal(Board,NX1,NY1, Player))).

checkRDDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX =< 9,
    NY =< 9,
    NY1 is NY + 1,
    NX1 is NX + 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkRDDiagonal(Board,NX1,NY1, Player))).


% checkLUDiagonal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces diagonally to the left and up of the pos
checkLUDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NY >= 0,
    NY1 is NY - 1,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkLUDiagonal(Board,NX1,NY1, Player))).

checkLUDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NY >= 0,
    NY1 is NY - 1,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkLUDiagonal(Board,NX1,NY1, Player))).


% checkLDDiagonal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces diagonally to the left and down of the pos
checkLDDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(black,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NY =< 9,
    NY1 is NY + 1,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 3 -> true ; checkLDDiagonal(Board,NX1,NY1, Player))).

checkLDDiagonal(Board,NX,NY, Player):-
    getRep(Player,X,_),
    getRep(white,Y,_),
    X == Y,
    getPiece(NY,NX,Board,Piece),
    getRep(Piece,_,Value),
    NX >= 0,
    NY =< 9,
    NY1 is NY + 1,
    NX1 is NX - 1,
    ((Value == 4; Value == 1) -> fail ; (Value == 2 -> true ; checkLDDiagonal(Board,NX1,NY1, Player))).    


testComp:-
    Tiece = none,
    getRep(Tiece,X,_),
    getRep(black,Y,_),
    X == Y.
