:- use_module(library(lists)).
:- use_module(library(random)).

getRep(none,'.').
getRep(white,'W').
getRep(black,'B').
getRep(wall,'G').
getRep(joker,'J').
getRep(bonus,'P').
getRep(_,_):-
    write('Unknown representation'),
    nl,
    fail.

printRep(L):-
    write(' '),
    write(L),
    write(' ').

/* creates a 10x10 empty board*/
createEmptyBoard(Board,N):-
   createBoard(Board,N).

createBoard([],0).
createBoard([Head|Tail],N):-
    createRow(Head,10),
    N1 is (N - 1),
    createBoard(Tail,N1).

createRow([],0).
createRow([Head|Tail],Line):-
    Head = none,
    Line1 is (Line - 1),
    createRow(Tail,Line1).

getPiece(Row, Column, Board, Piece) :-
        nth0(Row, Board, Row),
        nth0(Column, Row, Piece).

setPiece(Board,Col,Row,Piece,NewBoard):-
    nth0(Row,Board,RowLine,TempBoard),
    nth0(Col,RowLine,_,TempRow),
    nth0(Col,NewRow,Piece,TempRow),
    nth0(Row,NewBoard,NewRow,TempBoard).
    
setInitialPlayerPieces(Board,NewBoard):-
    setPiece(Board,4,4,white,B0),
    setPiece(B0,5,5,white,B1),
    setPiece(B1,4,5,black,B2),
    setPiece(B2,5,4,black,NewBoard).

setInitialPieces(Board,NewBoard):-
    setInitialPlayerPieces(Board,NewBoard).

printBoard(Board, N):-
    printHeader,
    printRows(Board, N).

printRows(_, 0).
printRows([FirstElem|OtherElem], N):-
    Number is 10 - N,
    printRowNumber(Number),
    N1 is N-1,
    printRow(FirstElem),
    printRowSep,
    printRows(OtherElem,N1).
    
printRow([]):-
    nl.
printRow([FirstElem|OtherElem]):-
    getRep(FirstElem,Label),
    printRep(Label),
    printColumnSep,
    printRow(OtherElem).

printRowNumber(N):-
    write(' '),
    write(N),
    write(' |').

printRowSep:-
    write('   |---|---|---|---|---|---|---|---|---|---|'),
    nl.

printHeader:-
    write('     0   1   2   3   4   5   6   7   8   9'), nl,
    printRowSep.

printColumnSep:-
    write('|').


