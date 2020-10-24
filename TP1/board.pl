:- use_module(library(lists)).
:- use_module(library(random)).

getRep(none,'.').
getRep(white,'W').
getRep(black,'B').
getRep(gray,'G').
getRep(yellow,'Y').
getRep(b_yellow,'BY').
getRep(w_yellow,'WY').
getRep(joker,'J').
getRep(_,_):-
    write('Unknown representation'),
    nl,
    fail.

printRep(L):-
    printColumnSep,
    write(L),
    printColumnSep.

/* creates a 10x10 empty board*/
createEmptyBoard(Board,N):-
   createBoard(Board,N,0).

createBoard(_,N,Lines).
createBoard([FirstRow|OtherRows],N,Lines):-
    Lines1 is (Lines + 1),
    createBoardLine(FirstRow,N),
    createBoard(OtherRows,N,Lines1).

createBoardLine(_,0).
createBoardLine([Elem1|OtherElem],N):-
    Elem1 = none,
    N1 is N - 1,
    createBoardLine(OtherElem,N1).


printBoard(Board, N):-
    printHeader,
    printRows(Board, N).


printRows(_, 0).
printRows([FirstElem|OtherElem], N):-
    printRowNumber(N),
    N1 is N-1,
    printRow(FirstElem),
    nl,
    printRowSep,
    printRows(OtherElem,N1).

    
printRow(_, 0).
printRow([FirstElem|OtherElem]):-
    getRep(FirstElem,Label),
    printRep(Label),
    printRow(OtherElem).

printRowNumber(N):-
    write(N),
    printColumnSep.

printRowSep:-
    write('|----|----|----|----|----|----|----|----|----|----|'),
    nl.

printHeader:-
    write('|--0-|--1-|--2-|--3-|--4-|--5-|--6-|--7-|--8-|--9-|'), nl,
    printRowSep.

printColumnSep:-
    write('|').

testBoard:-
    createEmptyBoard(B0,10),
    printBoard(Board,10).

