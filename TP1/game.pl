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
    playGame(B3, 0, 16, _, 0, 0).

% display_game(+Board, +Player)
% Displays the current game state, and announces next player turn
display_game(Board, Player):-
    printBoard(Board),
    write('Player '),
    write(Player),
    write(' Turn'), nl.

% makeTurn(+Board, +Player, -NewBoard)
% Goes through a player's turn on the game
makeTurn(Board, 0, NewBoard, Bonus):-
    placeDiscPlayer1(Board, NewBoard,IsBonus),
    Bonus is IsBonus,
    write(Bonus).

makeTurn(Board, 1, NewBoard, Bonus):-
    placeDiscPlayer2(Board, NewBoard,IsBonus),
    Bonus is IsBonus,
    write(Bonus).


printScore(Board,Player,Bonus):-
    (compare(=,Player,black) -> getBlackPlayerScore(Board, N) ;  getWhitePlayerScore(Board,N)),
    write('Without Bonus is '), write(N), nl,
    CurrentScore is N + Bonus,
    write('Current Player Score is '),
    write(CurrentScore), nl.

% playGame(+Board, +Turns, -NewBoard)
% Goes through each player's turn on the game
playGame(Board, _, 0, _ , _ , _):-
    printBoard(Board),
    write('Game Over!').

playGame(Board, Player, Turns, NewBoard, WScore, BScore):-
    NewTurns is Turns - 1,
    PlayerNum is Player mod 2,
    NewPlayer is Player + 1,
    display_game(Board, NewPlayer),
    makeTurn(Board, PlayerNum, TempBoard, Bonus),
    (compare(=,NewPlayer,1) -> BScore1 is BScore + Bonus , WScore1 is WScore , printScore(TempBoard,black,BScore1); WScore1 is WScore + Bonus, BScore1 is BScore, printScore(TempBoard,white,WScore1)),
    playGame(TempBoard, NewPlayer, NewTurns, NewBoard,WScore1,BScore1).

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
placeDiscPlayer1(Board, NewBoard, IsBonus):-
    readCoordinates('Disc', X, Y),
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,black)) -> (getPiece(Y,X,Board,Piece), setPiece(Board,X,Y,black,TempBoard), capturePieces(TempBoard, black, white, X, Y, NewBoard)) ; write('Invalid Move Try Again\n'), nl, placeDiscPlayer1(Board,NewBoard, IsBonus)),
    (compare(=,Piece,bonus) -> IsBonus is 5; IsBonus is 0).

% placeDiscPlayer2(+Board, -NewBoard)
% Places a white Disc (owned by player 2) on the board
placeDiscPlayer2(Board, NewBoard, IsBonus):-
    readCoordinates('Disc', X, Y),
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,white)) -> (getPiece(Y,X,Board,Piece),  setPiece(Board,X,Y,white,TempBoard), capturePieces(TempBoard, white, black, X, Y, NewBoard)) ; write('Invalid Move Try Again\n'), nl, placeDiscPlayer2(Board,NewBoard, IsBonus)),
    (compare(=,Piece,bonus) -> IsBonus is 5; IsBonus is 0).


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

% capturePieces(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks if there are capturable pieces in all possible directions, and captures them if possible
capturePieces(Board, Player, Capture, X, Y, NewBoard) :-
    capturePieceLeft(Board, Player, Capture, X, Y, TempBoard1),
    capturePieceRight(TempBoard1, Player, Capture, X, Y, TempBoard2),
    capturePieceUp(TempBoard2, Player, Capture, X, Y, TempBoard3),
    capturePieceDown(TempBoard3, Player, Capture, X, Y, TempBoard4),
    capturePieceLeftUp(TempBoard4, Player, Capture, X, Y, TempBoard5),
    capturePieceLeftDown(TempBoard5, Player, Capture, X, Y, TempBoard6),
    capturePieceRightUp(TempBoard6, Player, Capture, X, Y, TempBoard7),
    capturePieceRightDown(TempBoard7, Player, Capture, X, Y, NewBoard).

% capturePieceLeft(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly left for a piece of the opposite player, and captures it if possible
capturePieceLeft(Board, _, _, 1, _, Board).

capturePieceLeft(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X - 1,
    getPiece(Y,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X - 2,
    getPiece(Y,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y,Player,NewBoard).

capturePieceLeft(Board, _, _, _, _, Board).

% capturePieceRight(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly right for a piece of the opposite player, and captures it if possible
capturePieceRight(Board, _, _, 8, _, Board).

capturePieceRight(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X + 1,
    getPiece(Y,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X + 2,
    getPiece(Y,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y,Player,NewBoard).

capturePieceRight(Board, _, _, _, _, Board).

% capturePieceUp(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly up for a piece of the opposite player, and captures it if possible
capturePieceUp(Board, _, _, _, 1, Board).

capturePieceUp(Board, Player, Capture, X, Y, NewBoard) :-
    Y1 is Y - 1,
    getPiece(Y1,X,Board,Piece),
    compare(=, Piece, Capture),
    Y2 is Y - 2,
    getPiece(Y2,X,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X,Y1,Player,NewBoard).

capturePieceUp(Board, _, _, _, _, Board).

% capturePieceDown(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly down for a piece of the opposite player, and captures it if possible
capturePieceDown(Board, _, _, _, 8, Board).

capturePieceDown(Board, Player, Capture, X, Y, NewBoard) :-
    Y1 is Y + 1,
    getPiece(Y1,X,Board,Piece),
    compare(=, Piece, Capture),
    Y2 is Y + 2,
    getPiece(Y2,X,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X,Y1,Player,NewBoard).

capturePieceDown(Board, _, _, _, _, Board).

% capturePieceLeftUp(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly left and up for a piece of the opposite player, and captures it if possible
capturePieceLeftUp(Board, _, _, _, 1, Board).

capturePieceLeftUp(Board, _, _, 1, _, Board).

capturePieceLeftUp(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X - 1,
    Y1 is Y - 1,
    getPiece(Y1,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X - 2,
    Y2 is Y - 2,
    getPiece(Y2,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y1,Player,NewBoard).

capturePieceLeftUp(Board, _, _, _, _, Board).

% capturePieceLeftDown(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly left and down for a piece of the opposite player, and captures it if possible
capturePieceLeftDown(Board, _, _, _, 8, Board).

capturePieceLeftDown(Board, _, _, 1, _, Board).

capturePieceLeftDown(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X - 1,
    Y1 is Y + 1,
    getPiece(Y1,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X - 2,
    Y2 is Y + 2,
    getPiece(Y2,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y1,Player,NewBoard).

capturePieceLeftDown(Board, _, _, _, _, Board).

% capturePieceRightUp(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly right and up for a piece of the opposite player, and captures it if possible
capturePieceRightUp(Board, _, _, _, 1, Board).

capturePieceRightUp(Board, _, _, 8, _, Board).

capturePieceRightUp(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X + 1,
    Y1 is Y - 1,
    getPiece(Y1,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X + 2,
    Y2 is Y - 2,
    getPiece(Y2,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y1,Player,NewBoard).

capturePieceRightUp(Board, _, _, _, _, Board).

% capturePieceRightDown(+Board, +Player, +Capture, +X, +Y, -NewBoard)
% Checks directly right and down for a piece of the opposite player, and captures it if possible
capturePieceRightDown(Board, _, _, _, 8, Board).

capturePieceRightDown(Board, _, _, 8, _, Board).

capturePieceRightDown(Board, Player, Capture, X, Y, NewBoard) :-
    X1 is X + 1,
    Y1 is Y + 1,
    getPiece(Y1,X1,Board,Piece),
    compare(=, Piece, Capture),
    X2 is X + 2,
    Y2 is Y + 2,
    getPiece(Y2,X2,Board,Piece2),
    (compare(=, Piece2, Player) ; compare(=, Piece2, joker)),
    setPiece(Board,X1,Y1,Player,NewBoard).

capturePieceRightDown(Board, _, _, _, _, Board).

% validatePlay(+Board,+X,+Y,+Player)
% checks if play made by the player is valid
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

% canPlay(+Board, +Player)
% checks if the player can make any plays    
canPlay(Board,Player):-
    checkAllValidMoves(Board,PLayer,Points,0,0),
    length(Points,N),
    N1 is N - 1,
    (compare(=,N1,0) -> fail).

% checkAllValidMoves(+Board, +Player, -Table, +Y, +X)
% returns an array with all possible plays   
checkAllValidMoves(_,_,_,10,_).
checkAllValidMoves(Board, Player, [H|T], Y, X):-
    (validatePlay(Board,X,Y,Player) -> H = [X,Y],MOVE is 0 ; MOVE is 1),
    (compare(=,X,9) -> X1 is 0, Y1 is Y + 1; X1 is X + 1, Y1 is Y),
    (compare(=,MOVE,0) -> checkAllValidMoves(Board, Player, T, Y1, X1) ; (compare(=,Y1,10) -> checkAllValidMoves(Board, Player, T, Y1, X1); checkAllValidMoves(Board, Player, [H|T], Y1, X1) )).




% checkLeft(+Board,+X,+Y, +Player,+N)
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
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> \+compare(=, N, 0) ; 
    N1 is N + 1,
    checkLeft(Board,X1,Y, Player,N1)),
    !.

% checkRight(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces to the right of the pos        
checkRight(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    X =< 9,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRight(Board,X1,Y, Player,N1)),
    !.

% checkUp(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces to the above of the pos
checkUp(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    Y >= 0,
    Y1 is Y - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkUp(Board,X,Y1, Player,N1)),
    !. 

% checkDown(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces to the bellow of the pos
checkDown(Board,X,Y, Player,N):-
    getRep(Player,VX),
    getRep(black,VY),
    compare(=, VX, VY),
    Y =< 9,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkDown(Board,X,Y1, Player,N1)),
    !.

% checkLeftUp(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces diagonally up to the left of the pos
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
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftUp(Board,X1,Y1, Player,N1)),
    !. 

% checkLeftDown(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces diagonally down to the left of the pos
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
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkLeftDown(Board,X1,Y1, Player,N1)),
    !.

% checkRightUp(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces diagonally up to the right of the pos
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
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightUp(Board,X1,Y1, Player,N1)),
    !.

% checkRightUp(+Board,+X,+Y, +Player,+N)
% checks if play is possible by checking all pieces diagonally down to the right of the pos
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
    \+compare(=, Piece, bonus),
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
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker))  -> \+compare(=, N, 0)  ;
    N1 is N + 1,
    checkRightDown(Board,X1,Y1, Player,N1)),
    !.


% isGameOver(Board,X,Y,Skips)
% checks if the game is over
isGameOver(_,_,10,_).
isGameOver(_,_,_,2).
isGameOver(Board,X,Y,Skips):-
    checkRowEmptyPlaces(Board,X,Y),
    Skips < 2,
    Y1 is Y + 1,
    isGameOver(Board,X,Y,Skips).

% checkRowEmptyPlaces(+Board,+X,+Y)
% checks there is a cell empty in the row
checkRowEmptyPlaces(_,10,_).
checkRowEmptyPlaces(Board,X,Y):-
    getPiece(Y,X,Board,Piece),
    getRep(Piece, Rep),
    (compare(=, Rep, '.') -> fail),
    X1 is X + 1,
    checkRowEmptyPlaces(Board,X1,Y).
    

getBlackPlayerScore(Board,N):-
   sumBlackPlayerScores(0,0,Board,N).

getWhitePlayerScore(Board,N):-
    sumWhitePlayerScores(0,0,Board,N).


sumWhitePlayerScores(_,10,_,0).
sumWhitePlayerScores(X,Y,Board,Score):-
    Y1 is Y + 1,
    sumWhitePlayerScores(X,Y1,Board,AuxScore),
    getRowWhitePlayerScore(X,Y,Board,RowScore),
    Score is RowScore + AuxScore.
   
sumBlackPlayerScores(_,10,_,0).
sumBlackPlayerScores(X,Y,Board,Score):-
    Y1 is Y + 1,
    sumBlackPlayerScores(X,Y1,Board,AuxScore),
    getRowBlackPlayerScore(X,Y,Board,RowScore),
    Score is RowScore + AuxScore.

getRowWhitePlayerScore(10,_,_,0).
getRowWhitePlayerScore(X,Y,Board,Score):-
    X1 is X + 1,
    getRowWhitePlayerScore(X1,Y,Board,AuxScore),
    getPiece(Y,X,Board,Piece),
    getRep(Piece,'W'),
    Score is AuxScore + 1.
    
getRowBlackPlayerScore(10,_,_,0).
getRowBlackPlayerScore(X,Y,Board,Score):-
    X1 is X + 1,
    getRowBlackPlayerScore(X1,Y,Board,AuxScore),
    getPiece(Y,X,Board,Piece),
    getRep(Piece,'B'),
    Score is AuxScore + 1.

testCanMove:-
    initial(B0),
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    (canPlay(B2,black) -> write('yes')).


testValidMoves:-
    initial(B0),
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    checkAllValidMoves(B2,black,Points,0,0),
    write(Points),nl,
    length(Points, N),
    write(N),nl.
    
testCalculateScores:-
    initial(B0),
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    getBlackPlayerScore(B2,N1),
    getWhitePlayerScore(B2,N2),
    write('Test 1 = '), write(N1), nl,
    write('Test 2 = '), write(N2), nl.

testGameOver:-
    initial(B0),
    (isGameOver(B0,0,0,0) -> write('Game is Over') ; write('Game is not Over')).