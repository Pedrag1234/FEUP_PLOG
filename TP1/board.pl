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


printBoard(Board, N):-
    printHeader,
    printRows(Board, N).


printRows(_, 0).
printRows([FirstElem|OtherElem], N):-
    printRowNumber(N),
    N1 is N-1,
    printRow(FirstElem),
    printRowSep,
    printRows(OtherElem,N1).

    
printRow([]):-
    nl.
printRow([FirstElem|OtherElem]):-
    getRep(FirstElem,Label),
    printColumnSep,
    printRep(Label),
    printColumnSep,
    printRow(OtherElem).

printRowNumber(N):-
    write(N),
    printColumnSep.

printRowSep:-
    write('----|----|----|----|----|----|----|----|----|----|'),
    nl.

printHeader:-
    write('|--0-|--1-|--2-|--3-|--4-|--5-|--6-|--7-|--8-|--9-|'), nl,
    printRowSep.

printColumnSep:-
    write('|').

testBoard:-
    createEmptyBoard(B0,10),
    printBoard(B0,10).

