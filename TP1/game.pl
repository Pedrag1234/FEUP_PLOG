:- use_module(library(lists)).
:- include('board.pl').

startGame:-
    createEmptyBoard(B0,10),
    setInitialPieces(B0,B1),
    printBoard(B1,10),
    jokerSetupPhase(B1, 8, B2),
    wallSetupPhase(B2, 8, B3),
    bonusSetupPhase(B3, 8, B4).

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
    write('Piece Y Coordinate:'),
    readInput(Yinput), nl,
    nth0(0, Xinput, Xchar), 
    nth0(0, Yinput, Ychar), 
    number_chars(X, [Xchar]), number_chars(Y, [Ychar]).
    
jokerSetupPhase(Board, 0, Board).
jokerSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeJoker(Board, TempBoard),
    printBoard(TempBoard, 10),
    jokerSetupPhase(TempBoard, N1, NewBoard).

placeJoker(Board, NewBoard):-
     catch(readCoordinates('Joker', X, Y), _, fail),
     setPiece(Board, X, Y, joker, NewBoard).

wallSetupPhase(Board, 0, Board).
wallSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeWall(Board, TempBoard),
    printBoard(TempBoard, 10),
    wallSetupPhase(TempBoard, N1, NewBoard).

placeWall(Board, NewBoard):-
     catch(readCoordinates('Wall', X, Y), _, fail),
     setPiece(Board, X, Y, wall, NewBoard).

bonusSetupPhase(Board, 0, Board).
bonusSetupPhase(Board, N, NewBoard):-
    N > 0,
    N1 is N - 1,
    placeBonus(Board, TempBoard),
    printBoard(TempBoard, 10),
    bonusSetupPhase(TempBoard, N1, NewBoard).

placeBonus(Board, NewBoard):-
     catch(readCoordinates('Bonus', X, Y), _, fail),
     setPiece(Board, X, Y, bonus, NewBoard).
    
    
    
    
    

