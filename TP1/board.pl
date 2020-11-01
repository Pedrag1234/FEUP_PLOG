:- use_module(library(lists)).
:- use_module(library(random)).

% getRep(+Piece, -Label)
% Assigns a label to use on the board for each piece type
getRep(none,'.').
getRep(white,'W').
getRep(black,'B').
getRep(wall,'G').
getRep(joker,'J').
getRep(bonus,'P').
getRep(_,_).

% printRep(+Label)
% Displays a piece's representation on the board
printRep(L):-
    write(' '),
    write(L),
    write(' ').

% createEmptyBoard(-Board)
% Creates a new, empty, 10x10 board
createEmptyBoard(Board):-
   createBoard(Board, 10).

% createBoard(-Board, +N)
% Creates a list to hold the board 
createBoard([],0).
createBoard([Head|Tail],N):-
    createRow(Head,10),
    N1 is (N - 1),
    createBoard(Tail,N1).

% createRow(-RowList, +Line)
% Creates a row on the board
createRow([],0).
createRow([Head|Tail],Line):-
    Head = none,
    Line1 is (Line - 1),
    createRow(Tail,Line1).

% getPiece(+Row, +Column, +Board, -Piece)
% Returns a piece from a specific location on a given board
getPiece(Row, Column, Board, Piece) :-
    nth0(Row, Board, RowLine, _),
    nth0(Column, RowLine, Piece, _), !.

% setPiece(+Board, +Col, +Row, +Piece, -NewBoard)
% Changes a piece on a specific location on a given board
setPiece(Board,Col,Row,Piece,NewBoard):-
    nth0(Row,Board,RowLine,TempBoard),
    nth0(Col,RowLine,_,TempRow),
    nth0(Col,NewRow,Piece,TempRow),
    nth0(Row,NewBoard,NewRow,TempBoard).

% setInitialPieces(+Board, -NewBoard)
% Sets the 4 default black/white discs at the start of the game    
setInitialPieces(Board,NewBoard):-
    setPiece(Board,4,4,white,B0),
    setPiece(B0,5,5,white,B1),
    setPiece(B1,4,5,black,B2),
    setPiece(B2,5,4,black,NewBoard).

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

% printRowNumber(+N)
% Displays the number of the following row when displaying the board
printRowNumber(N):-
    write(' '),
    write(N),
    write(' |').

% printRowSep
% Displays the row separation line when displaying the board
printRowSep:-
    write('   |---|---|---|---|---|---|---|---|---|---|'),
    nl.

% printHeader
% Displays the board header
printHeader:-
    write('     0   1   2   3   4   5   6   7   8   9'), nl,
    printRowSep.

% printHeader
% Displays the column separation line when displaying the board
printColumnSep:-
    write('|').


