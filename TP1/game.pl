:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- include('board.pl').

startGame:-
    createEmptyBoard(B0,10),
    setInitialPieces(B0,B1),
    printBoard(B1,10),
    jokerSetupPhase(B1, 8, B2),
    write('Performing random Wall and Bonus pieces placement'), nl,
    wallSetupPhase(B2, 8, B3),
    bonusSetupPhase(B3, 8, B4),
    printBoard(B4, 10),
    playGame(B4, 2, _),
    write('Game Over').

playGame(_, 0, _).
playGame(Board, Turns, NewBoard):-
    write('Player 1 Turn'), nl,
    placeDiscPlayer1(Board, TempBoard),
    printBoard(TempBoard, 10),
    write('Player 2 Turn'), nl,
    placeDiscPlayer2(TempBoard, TempBoard2),
    printBoard(TempBoard2, 10),
    NewTurns is Turns - 1,
    playGame(TempBoard2, NewTurns, NewBoard).
    
readInput(Input):-
        get_char(Char),
        readEnter(Char, Input).

readEnter('\n', []).
readEnter(Char, [Char|Rest]) :-
        get_char(Char1),
        readEnter(Char1, Rest).

readCoordinates(PieceStr, X, Y):-
    write(PieceStr), write(' X Coordinate:'),
    readInput(Xinput), nl,
    write(PieceStr), write(' Y Coordinate:'),
    readInput(Yinput), nl,
    nth0(0, Xinput, Xchar), 
    nth0(0, Yinput, Ychar),
    number_chars(X, [Xchar]), number_chars(Y, [Ychar]).

% trying to prevent invalid inputs, not finished
%readCoordinates(PieceStr, X, Y):-
%    write('\nInvalid input! Try Again\n'), nl,
%    readCoordinates(PieceStr, X, Y).
%

checkInput('Joker', X, Y) :-
    between(0, 9, X),
    (Y == 0; Y == 9).

checkInput('Joker', X, Y) :-
    between(0, 9, Y),
    (X == 0; X == 9).

checkInput('Disc', X, Y) :-
    between(1, 8, X),
    between(1, 8, Y).

validateJokerInput(Xinput,Yinput):-
    checkInput('Joker',Xinput,Yinput).

validateDiscInput(Xinput,Yinput):-
    checkInput('Disc',Xinput,Yinput).

checkPlace(Board, X, Y) :-
    getPiece(X, Y, Board, Piece),
    getRep(Piece,Label),
    if(Label == '.', true, fail).

    
    
jokerSetupPhase(Board, 0, Board).
jokerSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeJoker(Board, TempBoard),
    printBoard(TempBoard, 10),
    jokerSetupPhase(TempBoard, N1, NewBoard).
     
wallSetupPhase(Board, 0, Board).
wallSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    (placeWall(Board, TempBoard) -> wallSetupPhase(TempBoard, N1, NewBoard) ; wallSetupPhase(Board, N, NewBoard)).

placeWall(Board, NewBoard):-
     random(1, 8, X),
     random(1, 8, Y),
     (checkPlace(Board,X,Y) == true -> setPiece(Board, X, Y, wall, NewBoard) ; fail).

bonusSetupPhase(Board, 0, Board).
bonusSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeBonus(Board, TempBoard),
    bonusSetupPhase(TempBoard, N1, NewBoard).

placeDiscPlayer1(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    (validateDiscInput(X,Y) -> setPiece(Board,X,Y,black,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer1(Board,NewBoard)).

placeDiscPlayer2(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    (validateDiscInput(X,Y) -> setPiece(Board,X,Y,white,NewBoard) ; write('Discs must be placed in the inner 8x8 square, input again\n'), nl, placeDiscPlayer2(Board,NewBoard)).

placeJoker(Board, NewBoard):-
    readCoordinates('Joker', X, Y),
    (validateJokerInput(X,Y) -> setPiece(Board,X,Y,joker,NewBoard) ; write('Jokers must be placed on the outer border, input again\n'), nl, placeJoker(Board,NewBoard)).

placeWall(Board, NewBoard):-
     random(1, 8, X),
     random(1, 8, Y),
     setPiece(Board, X, Y, wall, NewBoard).

placeBonus(Board, NewBoard):-
     random(1, 8, X),
     random(1, 8, Y),
     setPiece(Board, X, Y, bonus, NewBoard).
    
    
    
    
    

