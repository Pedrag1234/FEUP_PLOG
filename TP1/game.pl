:- use_module(library(lists)).
:- use_module(library(between)).
:- use_module(library(random)).
:- include('board.pl').

startGame:-
    createEmptyBoard(B0,10),
    setInitialPieces(B0,B1),
    printBoard(B1,10),
    jokerSetupPhase(B1, 8, B2),
    %wallSetupPhase(B2, 8, B3),
    %bonusSetupPhase(B3, 8, B4),
    printBoard(B4, 10).

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

checkInput('Joker', 0, X, Y) :-
    between(0, 9, X),
    (Y == 0; Y == 9).

checkInput('Joker', 0, X, Y) :-
    between(0, 9, Y),
    (X == 0; X == 9).

%checkInput('Joker', _, _) :-
%    write('Jokers must be placed on the outer border, input again\n'), nl, fail.

validateJokerInput(Xinput,Yinput):-
    checkInput('Joker',0,Xinput,Yinput).


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

placeJoker(Board, NewBoard):-
    readCoordinates('Joker', X, Y),
    (validateJokerInput(X,Y) -> setPiece(Board,X,Y,joker,NewBoard) ; write('Jokers must be placed on the outer border, input again\n'), nl, placeJoker(Board,NewBoard)).
     

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

placeBonus(Board, NewBoard):-
     random(1, 8, X),
     random(1, 8, Y),
     setPiece(Board, X, Y, bonus, NewBoard).
    
    
    
    
    

