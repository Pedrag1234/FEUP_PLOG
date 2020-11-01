:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- use_module(library(system)).
:- include('board.pl').

% play
% Starts a new game
play:-
    now(N),
    Seed is ((N mod 30265) + 1),
    setrand(random(Seed, Seed, Seed, N)),
    createEmptyBoard(B0),
    setInitialPieces(B0,B1),
    printBoard(B1),
    jokerSetupPhase(B1, 0, B2),
    write('Performing random Wall and Bonus pieces placement'), nl,
    wallSetupPhase(B2, 8, B3),
    bonusSetupPhase(B3, 8, B4),
    printBoard(B4),
    playGame(B4, 2, _),
    write('Game Over').

% playGame(+Board, +Turns, -NewBoard)
% Goes through each player's turn on the game
playGame(_, 0, _).
playGame(Board, Turns, NewBoard):-
    write('Player 1 Turn'), nl,
    placeDiscPlayer1(Board, TempBoard),
    printBoard(TempBoard),
    write('Player 2 Turn'), nl,
    placeDiscPlayer2(TempBoard, TempBoard2),
    printBoard(TempBoard2),
    NewTurns is Turns - 1,
    playGame(TempBoard2, NewTurns, NewBoard).

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
    between(1, 8, X),
    between(1, 8, Y).

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
    getRep(Piece,Label),
    if(Label == '.', true, fail).

% jokerSetupPhase(+Board, +N, -NewBoard)
% Sets up the initial Jokers on the board
jokerSetupPhase(Board, 0, Board).
jokerSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeJoker(Board, TempBoard),
    printBoard(TempBoard),
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
    (validateDiscInput(X,Y) -> setPiece(Board,X,Y,black,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer1(Board,NewBoard)).

% placeDiscPlayer2(+Board, -NewBoard)
% Places a white Disc (owned by player 2) on the board
placeDiscPlayer2(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    (validateDiscInput(X,Y) -> setPiece(Board,X,Y,white,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer2(Board,NewBoard)).

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
    
    
    
    
    

