:- use_module(library(lists)).
:- include('board.pl').

startGame:-
    createEmptyBoard(B0,10),
    setInitialPieces(B0,B1),
    printBoard(B1,10),
    setupPhase(B1, 8, B2),
    printBoard(B2,10).

readInput(Input):-
        get_char(Char),
        readEnter(Char, Input).

readEnter('\n', []).
readEnter(Char, [Char|Rest]) :-
        get_char(Char1),
        readEnter(Char1, Rest).

readCoordinates(X, Y):-
    write('Piece X Coordinate:'),
    readInput(Xinput), nl,
    write('Piece Y Coordinate:'),
    readInput(Yinput), nl,
    nth0(0, Xinput, Xchar), 
    nth0(0, Yinput, Ychar), 
    number_chars(X, [Xchar]), number_chars(Y, [Ychar]).
    
setupPhase(Board, 0, Board).
setupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeJoker(Board, TempBoard),
    printBoard(TempBoard, 10),
    setupPhase(TempBoard, N1, NewBoard).

placeJoker(Board, NewBoard):-
     catch(readCoordinates(X, Y), _, fail),
     setPiece(Board, X, Y, joker, NewBoard).
    
    
    
    
    

