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
    jokerSetupPhase(B2, 1, B3),
    getUpPiece(B3, white, 4, 7, YPiece, Found), nl,
    write(Found), nl,
    write(YPiece), nl,
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
    getRep(Piece,Label),
    Label == '.'.

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

capturePieceHorizontal(Board, _, _, X, X, _, Board).

capturePieceHorizontal(Board, Player, Capture, X, XLimit, Y, NewBoard) :-
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=, Piece, Capture) -> (setPiece(Board,X,Y,Player,TempBoard), capturePieceHorizontal(TempBoard, Player, Capture, X1, XLimit, Y, NewBoard));
    capturePieceHorizontal(Board, Player, Capture, X1, XLimit, Y, NewBoard)).

capturePieceVertical(Board, _, _, _, Y, Y, Board).

capturePieceVertical(Board, Player, Capture, X, Y, YLimit, NewBoard) :-
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=, Piece, Capture) -> (setPiece(Board,X,Y,Player,NewBoard), capturePieceVertical(Board, Player, Capture, X, Y1, YLimit, NewBoard));
    capturePieceVertical(Board, Player, Capture, X, Y1, YLimit, NewBoard)).

capturePieceDiagonalDownRight(Board, _, _, X, X, Y, Y, Board).

capturePieceDiagonalDownRight(Board, Player, Capture, X, XLimit, Y, YLimit, NewBoard) :-
    X1 is X + 1,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=, Piece, Capture) -> (setPiece(Board,X,Y,Player,TempBoard), capturePieceDiagonalDownRight(TempBoard, Player, Capture, X1, XLimit, Y1, YLimit, NewBoard));
    capturePieceDiagonalDownRight(Board, Player, Capture, X1, XLimit, Y1, YLimit, NewBoard)).

capturePieceDiagonalDownLeft(Board, _, _, X, X, Y, Y, Board).

capturePieceDiagonalDownLeft(Board, Player, Capture, X, XLimit, Y, YLimit, NewBoard) :-
    X1 is X - 1,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=, Piece, Capture) -> (setPiece(Board,X,Y,Player,TempBoard), capturePieceDiagonalDownLeft(TempBoard, Player, Capture, X1, XLimit, Y1, YLimit, NewBoard));
    capturePieceDiagonalDownLeft(Board, Player, Capture, X1, XLimit, Y1, YLimit, NewBoard)).

getLeftPiece(_, _, 1, _, _, 0).
                      
getLeftPiece(Board, Player, X, Y, XPiece, Found) :-
    X1 is X - 1,
    getPiece(Y, X1, Board, Piece),
    (compare(=, Piece, Player) -> (XPiece is X1, Found is 1);
    getLeftPiece(Board, Player, X1, Y, XPiece, Found)).

getRightPiece(_, _, 8, _, _, 0).
                      
getRightPiece(Board, Player, X, Y, XPiece, Found) :-
    X1 is X + 1,
    getPiece(Y, X1, Board, Piece),
    (compare(=, Piece, Player) -> (XPiece is X1, Found is 1);
    getRightPiece(Board, Player, X1, Y, XPiece, Found)).

getDownPiece(_, _, _, 8, _, 0).
                      
getDownPiece(Board, Player, X, Y, YPiece, Found) :-
    Y1 is Y + 1,
    getPiece(Y1, X, Board, Piece),
    (compare(=, Piece, Player) -> (YPiece is Y1, Found is 1);
    getDownPiece(Board, Player, X, Y1, YPiece, Found)).

getUpPiece(_, _, _, 1, _, 0).
                      
getUpPiece(Board, Player, X, Y, YPiece, Found) :-
    Y1 is Y - 1,
    getPiece(Y1, X, Board, Piece),
    (compare(=, Piece, Player) -> (YPiece is Y1, Found is 1);
    getUpPiece(Board, Player, X, Y1, YPiece, Found)).


validatePlay(Board,X,Y,Player):-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    (checkLeft(Board,X1,Y, Player,0);
    checkRight(Board,X2,Y, Player,0);
    checkUp(Board,X,Y1, Player,0);
    checkDown(Board,X,Y2, Player,0);
    checkLeftUp(Board,X1,Y1, Player,0);
    checkLeftDown(Board,X1,Y2, Player,0);
    checkRightUp(Board,X2,Y1, Player,0);
    checkRightDown(Board,X2,Y2, Player,0)).
    


canPlay(Board,Player).

checkAllValidMoves(_,_,_,10,_).
checkAllValidMoves(Board, Player, [H|T], Y, X):-
    checkRowAllValidMoves(Board,Player,H,Y,X),
    write('Y = '),write(Y),nl,
    write(H),nl,
    Y1 is Y + 1,
    checkAllValidMoves(Board, Player, T, Y1, X).

checkRowAllValidMoves(_,_,_,_,10).
checkRowAllValidMoves(Board, Player, [H|T], Y, X):-
    X1 is X + 1,
    Point = [X,Y],
    (validatePlay(Board,X,Y,Player) -> H = Point , checkRowAllValidMoves(Board,Player,T,Y,X1) ; H = null , checkRowAllValidMoves(Board,Player,T,Y,X1)).
    

% checkLHorizontal(+Board,+NX,+NY,+Player)
% checks if play is possible by checking all pieces to the left of the pos
checkLeft(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X >= 0,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeft(Board,X1,Y, Player,N1)),
    !.

checkLeft(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X >= 0,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> \+compare(=, N, 0) ; 
    N1 is N + 1,
    checkLeft(Board,X1,Y, Player,N1)),
    !.
        
checkRight(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X =< 9,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRight(Board,X1,Y, Player,N1)),
    !.

checkRight(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X =< 9,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRight(Board,X1,Y, Player,N1)),
    !.

checkUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    Y >= 0,
    Y1 is Y - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkUp(Board,X,Y1, Player,N1)),
    !.

checkUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    Y >= 0,
    Y1 is Y - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkUp(Board,X,Y1, Player,N1)),
    !. 

checkDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    Y =< 9,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkDown(Board,X,Y1, Player,N1)),
    !.

checkDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    Y =< 9,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkDown(Board,X,Y1, Player,N1)),
    !.

checkLeftUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X >= 0,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftUp(Board,X1,Y1, Player,N1)),
    !.

checkLeftUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X >= 0,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftUp(Board,X1,Y1, Player,N1)),
    !. 

checkLeftDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X >= 0,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftDown(Board,X1,Y1, Player,N1)),
    !.

checkLeftDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X >= 0,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftDown(Board,X1,Y1, Player,N1)),
    !.

checkRightUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X =< 9,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightUp(Board,X1,Y1, Player,N1)),
    !.

checkRightUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X =< 9,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightUp(Board,X1,Y1, Player,N1)),
    !.

checkRightDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X =< 9,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, black) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightDown(Board,X1,Y1, Player,N1)),
    !.

checkRightDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(white,VY),
    compare(=, VX, VY),
    X =< 9,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightDown(Board,X1,Y1, Player,N1)),
    !.


isGameOver(_,_,10,_).
isGameOver(_,_,_,2).

isGameOver(Board,X,Y,Skips):-
    checkRowEmptyPlaces(Board,X,Y),
    Skips < 2,
    Y1 is Y + 1,
    isGameOver(Board,X,Y,Skips).

checkRowEmptyPlaces(_,10,_).

checkRowEmptyPlaces(Board,X,Y):-
    getPiece(Y,X,Board,Piece),
    getRep(Piece, Rep),
    (compare(=, Rep, '.') -> fail),
    X1 is X + 1,
    checkRowEmptyPlaces(Board,X1,Y).
    

testValidMoves:-
    initial(B0),
    checkAllValidMoves(B0,black,Points,0,0),
    write(Points).

testGameOver:-
    initial(B0),
    (isGameOver(B0,0,0,0) -> write('Game is Over') ; write('Game is not Over')).