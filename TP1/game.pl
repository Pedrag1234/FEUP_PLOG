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
    nl, write('###############'), nl,
    write('##  Mapello  ##'), nl,
    write('###############'), nl, nl,
    write('1 - Player vs Player'), nl,
    write('2 - Player vs CPU'), nl,
    write('3 - CPU vs CPU'), nl, nl,
    write('Select game mode: '),
    readOption(Input),
    ((compare(=, Input, 1), nl, setupPvP);
    (compare(=, Input, 2), nl, setupPvC);
    (compare(=, Input, 3), nl, setupCvC);
    (nl, write('Please input 1, 2 or 3, to select the game mode'), nl, play)).

% setupPvP
% Starts a new Player vs Player game
setupPvP:-
    initRandom,
    initial(B0),
    nl, write('Performing random Wall and Bonus pieces placement'), nl,
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    jokerSetupPhase(B2, 1, B3),
    playPvPGame(B3, 0, 16, _).

% chooseCPUSide(-Side)
% User decides whether the CPU controls black or white discs
chooseCPUSide(Side):-
    write('CPU Side:'), nl, nl,
    write('1 - Black'), nl,
    write('2 - White'), nl, nl,
    write('Select CPU side: '),
    readOption(Input),
    ((compare(=, Input, 1), Side is 0);
    (compare(=, Input, 2), Side is 1);
    (nl, write('Please input 1 or 2, to select the CPU side'), nl, chooseCPUSide(Side))).

% chooseCPUDifficulty(+Text, -Difficulty)
% User chooses the difficulty of the CPU (easy/medium)
chooseCPUDifficulty(Text, Difficulty):-
    write('CPU'), write(Text), write('Difficulty:'), nl, nl,
    write('1 - Easy'), nl,
    write('2 - Medium'), nl, nl,
    write('Select CPU difficulty: '),
    readOption(Input),
    ((compare(=, Input, 1), Difficulty is 1);
    (compare(=, Input, 2), Difficulty is 2);
    (nl, write('Please input 1 or 2, to select the CPU difficulty'), nl, chooseCPUDifficulty(Text, Difficulty))).

% setupPvC
% Starts a new Player vs CPU game
setupPvC:-
    chooseCPUSide(Side), nl,
    chooseCPUDifficulty(' ', Difficulty), nl,
    initRandom,
    initial(B0),
    nl, write('Performing random Wall and Bonus pieces placement'), nl,
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    jokerSetupPhase(B2, 1, B3),
    playPvCGame(B3, 0, Side, Difficulty, 16, _).

% setupCvC
% Starts a new CPU vs CPU game
setupCvC:-
    chooseCPUDifficulty(' 1 (Black) ', Difficulty1), nl,
    chooseCPUDifficulty(' 2 (White) ', Difficulty2), nl,
    initRandom,
    initial(B0),
    nl, write('Performing random Wall and Bonus pieces placement'), nl,
    wallSetupPhase(B0, 8, B1),
    bonusSetupPhase(B1, 8, B2),
    jokerSetupPhase(B2, 1, B3),
    playCvCGame(B3, 0, Difficulty1, Difficulty2, 16, _).

% display_game(+Board, +Player)
% Displays the current game state, and announces next player turn
display_game(Board, Player):-
    printBoard(Board),
    write('Player '),
    write(Player),
    write(' Turn'), nl.

% makePlayerTurn(+Board, +Player, -NewBoard)
% Goes through a player's turn on the game
makePlayerTurn(Board, 0, NewBoard):-
    placeDiscPlayer1(Board, NewBoard).

makePlayerTurn(Board, 1, NewBoard):-
    placeDiscPlayer2(Board, NewBoard).

% makeCPUTurn(+Board, +Player, +Difficulty -NewBoard)
% Goes through a CPU's turn on the game
makeCPUTurn(Board, 0, Difficulty, NewBoard):-
    sleep(2),
    placeDiscCPU1(Board, Difficulty, NewBoard).

makeCPUTurn(Board, 1, Difficulty, NewBoard):-
    sleep(2),
    placeDiscCPU2(Board, Difficulty, NewBoard).

% playPvPGame(+Board, +Player, +Turns, -NewBoard)
% Goes through each player's turn on the game
playPvPGame(Board, _, 0, _,_):-
    printBoard(Board),
    write('Game Over!').

playPvPGame(Board, Player, Turns, NewBoard, Skips):-
    NewTurns is Turns - 1,
    PlayerNum is Player mod 2,
    NewPlayer is Player + 1,
    PlayerDisplay is PlayerNum + 1,
    display_game(Board, PlayerDisplay),
    (makePlayerTurn(Board, PlayerNum, TempBoard) -> Skips1 is Skips + 1 ; Skips1 is 0),
    (isGameOver(Board,0,0,Skips) , playPvPGame(TempBoard, NewPlayer, 0, NewBoard,Skips1)); 
    playPvPGame(TempBoard, NewPlayer, NewTurns, NewBoard,Skips1).

% playPvCGame(+Board, +Player, +CPUSide, +CPUDifficulty +Turns, -NewBoard)
% Alternates through the player and the CPU's turn on the game
playPvCGame(Board, _, _, _, 0, _):-
    printBoard(Board),
    write('Game Over!').

playPvCGame(Board, Player, CPUSide, CPUDifficulty, Turns, NewBoard):-
    NewTurns is Turns - 1,
    PlayerNum is Player mod 2,
    NewPlayer is Player + 1,
    PlayerDisplay is PlayerNum + 1,
    display_game(Board, PlayerDisplay),
    ((compare(=, PlayerNum, CPUSide), makeCPUTurn(Board, PlayerNum, CPUDifficulty, TempBoard));
    makePlayerTurn(Board, PlayerNum, TempBoard)),
    playPvCGame(TempBoard, NewPlayer, CPUSide, CPUDifficulty, NewTurns, NewBoard).

% playCvCGame(+Board, +Player, +CPU1Difficulty, +CPU2Difficulty, +Turns, -NewBoard)
% Goes through each CPU's turn on the game
playCvCGame(Board, _, _, _, 0, _):-
    printBoard(Board),
    write('Game Over!').

playCvCGame(Board, Player, CPU1Difficulty, CPU2Difficulty, Turns, NewBoard):-
    NewTurns is Turns - 1,
    PlayerNum is Player mod 2,
    NewPlayer is Player + 1,
    PlayerDisplay is PlayerNum + 1,
    display_game(Board, PlayerDisplay),
    ((compare(=, PlayerNum, 0), makeCPUTurn(Board, PlayerNum, CPU1Difficulty, TempBoard));
    makeCPUTurn(Board, PlayerNum, CPU2Difficulty, TempBoard)),
    playCvCGame(TempBoard, NewPlayer, CPU1Difficulty, CPU2Difficulty, NewTurns, NewBoard).

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

% readOption(-Option)
% Reads game mode option input by the player, used to initiate the game
readOption(Option):-
    readInput(Input),
    nth0(0, Input, Inputchar), 
    number_chars(Option, [Inputchar]).

% checkInput(+PieceStr, +X, +Y)
% Checks if the coordinates used are valid for the given piece
checkInput('Joker', X, Y) :-
    between(0, 9, X),
    (Y == 0; Y == 9).

checkInput('Joker', X, Y) :-
    between(0, 9, Y),
    (X == 0; X == 9).

checkInput('Disc', X, Y) :-
    between(1, 9, X),
    between(1, 9, Y).

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
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,black)) -> (setPiece(Board,X,Y,black,TempBoard), capturePieces(TempBoard, black, X, Y, NewBoard)) ; write('Invalid move, can\'t place discs on top of walls/discs, discs must be in the inner 8x8 square, and they must capture an enemy piece\n'), nl, placeDiscPlayer1(Board,NewBoard)).

% placeDiscPlayer2(+Board, -NewBoard)
% Places a white Disc (owned by player 2) on the board
placeDiscPlayer2(Board, NewBoard):-
    readCoordinates('Disc', X, Y),
    ((validateDiscInput(X,Y),validatePlay(Board,X,Y,white)) -> (setPiece(Board,X,Y,white,TempBoard), capturePieces(TempBoard, white, X, Y, NewBoard)) ; write('Invalid move, can\'t place discs on top of walls/discs, discs must be in the inner 8x8 square, and they must capture an enemy piece\n'), nl, placeDiscPlayer2(Board,NewBoard)).

% getMove(+ValidMoves, +MoveNumber, -Move)
% Gets a selected move from the valid moves list.
getMove([ValidMove|_], 1, ValidMove).
getMove([_|T], MoveNumber, Move):-
    NextMove is MoveNumber - 1,
    getMove(T, NextMove, Move).

% getBestMove(+Board, +ValidMoves, +Player, +BestCaptures, +StoredMove, -BestMove)
% Gets the best possible move (move that would result in the most piece captures) from the valid moves list.
getBestMove(_, [_|[]], _, _, BestMove, BestMove).
getBestMove(Board, [Move|T], Player, BestCaptures, StoredMove, BestMove):-
    Move = [X,Y],
    checkTotalCaptures(Board, Player, X, Y, Captures),
    ((Captures > BestCaptures, getBestMove(Board, T, Player, Captures, Move, BestMove));
    getBestMove(Board, T, Player, BestCaptures, StoredMove, BestMove)).

% placeDiscCPU1(+Board, +Difficulty, -NewBoard)
% Places a black Disc (owned by a CPU player 1) on the board
placeDiscCPU1(Board, 1, NewBoard):-
    canPlay(Board, black, ValidMoves),
    length(ValidMoves, MovesAmount),
    random(1, MovesAmount, MoveNumber),
    getMove(ValidMoves, MoveNumber, Move),
    Move = [X,Y],
    setPiece(Board, X, Y, black, TempBoard),
    capturePieces(TempBoard, black, X, Y, NewBoard).

placeDiscCPU1(Board, 2, NewBoard):-
    canPlay(Board, black, ValidMoves),
    getBestMove(Board, ValidMoves, black, 0, _, BestMove),
    BestMove = [X,Y],
    setPiece(Board, X, Y, black, TempBoard),
    capturePieces(TempBoard, black, X, Y, NewBoard).

% placeDiscCPU2(+Board, +Difficulty, -NewBoard)
% Places a white Disc (owned by a CPU player 2) on the board
placeDiscCPU2(Board, 1, NewBoard):-
    canPlay(Board, white, ValidMoves),
    length(ValidMoves, MovesAmount),
    random(1, MovesAmount, MoveNumber),
    getMove(ValidMoves, MoveNumber, Move),
    Move = [X,Y],
    setPiece(Board, X, Y, white, TempBoard),
    capturePieces(TempBoard, white, X, Y, NewBoard).

placeDiscCPU2(Board, 2, NewBoard):-
    canPlay(Board, white, ValidMoves),
    getBestMove(Board, ValidMoves, white, 0, _, BestMove),
    BestMove = [X,Y],
    setPiece(Board, X, Y, white, TempBoard),
    capturePieces(TempBoard, white, X, Y, NewBoard).

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

% checkTotalCaptures(+Board, +Player, +X, +Y, -Captures)
% Returns the amount of enemy piece captures that would occur after placing a piece on a given position
checkTotalCaptures(Board, Player, X, Y, Captures) :-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    checkLeft(Board, X1, Y, Player, 0, Pieces1),
    checkRight(Board, X2, Y, Player, 0, Pieces2),
    checkUp(Board, X, Y1, Player, 0, Pieces3),
    checkDown(Board, X, Y2, Player, 0, Pieces4),
    checkLeftUp(Board, X1, Y1, Player, 0, Pieces5),
    checkLeftDown(Board, X1, Y2, Player, 0, Pieces6),
    checkRightUp(Board, X2, Y1, Player, 0, Pieces7),
    checkRightDown(Board, X2, Y2, Player, 0, Pieces8),
    Captures is (Pieces1 + Pieces2 + Pieces3 + Pieces4 + Pieces5 + Pieces6 + Pieces7 + Pieces8).

% capturePieces(+Board, +Player, +X, +Y, -NewBoard)
% Checks if there are capturable pieces in all possible directions, and captures them if possible
capturePieces(Board, Player, X, Y, NewBoard) :-
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    checkLeft(Board, X1, Y, Player, 0, Pieces1), LeftTargetX is X - Pieces1,
    checkRight(Board, X2, Y, Player, 0, Pieces2), RightTargetX is X + Pieces2,
    checkUp(Board, X, Y1, Player, 0, Pieces3), UpTargetY is Y - Pieces3,
    checkDown(Board, X, Y2, Player, 0, Pieces4), DownTargetY is Y + Pieces4,
    checkLeftUp(Board, X1, Y1, Player, 0, Pieces5), LeftUpTargetX is X - Pieces5,
    checkLeftDown(Board, X1, Y2, Player, 0, Pieces6), LeftDownTargetX is X - Pieces6,
    checkRightUp(Board, X2, Y1, Player, 0, Pieces7), RightUpTargetX is X + Pieces7,
    checkRightDown(Board, X2, Y2, Player, 0, Pieces8), RightDownTargetX is X + Pieces8,
    capturePieceLeft(Board, Player, X, Y, LeftTargetX, TempBoard1),
    capturePieceRight(TempBoard1, Player, X, Y, RightTargetX, TempBoard2),
    capturePieceUp(TempBoard2, Player, X, Y, UpTargetY, TempBoard3),
    capturePieceDown(TempBoard3, Player, X, Y, DownTargetY, TempBoard4),
    capturePieceLeftUp(TempBoard4, Player, X, Y, LeftUpTargetX, TempBoard5),
    capturePieceLeftDown(TempBoard5, Player, X, Y, LeftDownTargetX, TempBoard6),
    capturePieceRightUp(TempBoard6, Player, X, Y, RightUpTargetX, TempBoard7),
    capturePieceRightDown(TempBoard7, Player, X, Y, RightDownTargetX, NewBoard).

% capturePieceLeft(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the left of the given piece
capturePieceLeft(Board, _, TargetX, _, TargetX, Board).

capturePieceLeft(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X - 1,
    setPiece(Board,NextX,Y,Player,TempBoard),
    capturePieceLeft(TempBoard, Player, NextX, Y, TargetX, NewBoard).

% capturePieceRight(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the right of the given piece
capturePieceRight(Board, _, TargetX, _, TargetX, Board).

capturePieceRight(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X + 1,
    setPiece(Board,NextX,Y,Player,TempBoard),
    capturePieceRight(TempBoard, Player, NextX, Y, TargetX, NewBoard).

% capturePieceUp(+Board, +Player, +X, +Y, +TargetY, -NewBoard)
% Captures a specified amount of enemy pieces located above the given piece
capturePieceUp(Board, _, _, TargetY, TargetY, Board).

capturePieceUp(Board, Player, X, Y, TargetY, NewBoard) :-
    NextY is Y - 1,
    setPiece(Board,X,NextY,Player,TempBoard),
    capturePieceUp(TempBoard, Player, X, NextY, TargetY, NewBoard).

% capturePieceDown(+Board, +Player, +X, +Y, +TargetY, -NewBoard)
% Captures a specified amount of enemy pieces located below the given piece
capturePieceDown(Board, _, _, TargetY, TargetY, Board).

capturePieceDown(Board, Player, X, Y, TargetY, NewBoard) :-
    NextY is Y + 1,
    setPiece(Board,X,NextY,Player,TempBoard),
    capturePieceDown(TempBoard, Player, X, NextY, TargetY, NewBoard).

% capturePieceLeftUp(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the left and above the given piece
capturePieceLeftUp(Board, _, TargetX, _, TargetX, Board).

capturePieceLeftUp(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X - 1,
    NextY is Y - 1,
    setPiece(Board,NextX,NextY,Player,TempBoard),
    capturePieceLeftUp(TempBoard, Player, NextX, NextY, TargetX, NewBoard).

% capturePieceLeftDown(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the left and below the given piece
capturePieceLeftDown(Board, _, TargetX, _, TargetX, Board).

capturePieceLeftDown(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X - 1,
    NextY is Y + 1,
    setPiece(Board,NextX,NextY,Player,TempBoard),
    capturePieceLeftDown(TempBoard, Player, NextX, NextY, TargetX, NewBoard).

% capturePieceRightUp(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the right and above the given piece
capturePieceRightUp(Board, _, TargetX, _, TargetX, Board).

capturePieceRightUp(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X + 1,
    NextY is Y - 1,
    setPiece(Board,NextX,NextY,Player,TempBoard),
    capturePieceRightUp(TempBoard, Player, NextX, NextY, TargetX, NewBoard).

% capturePieceRightDown(+Board, +Player, +X, +Y, +TargetX, -NewBoard)
% Captures a specified amount of enemy pieces located to the right and below the given piece
capturePieceRightDown(Board, _, TargetX, _, TargetX, Board).

capturePieceRightDown(Board, Player, X, Y, TargetX, NewBoard) :-
    NextX is X + 1,
    NextY is Y + 1,
    setPiece(Board,NextX,NextY,Player,TempBoard),
    capturePieceRightDown(TempBoard, Player, NextX, NextY, TargetX, NewBoard).

% validatePlay(+Board,+X,+Y,+Player)
% checks if play made by the player is valid
validatePlay(Board,X,Y,Player):-
    checkInput('Disc', X, Y),
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, black),
    \+compare(=, Piece, white),
    \+compare(=, Piece, wall),
    X1 is X - 1,
    X2 is X + 1,
    Y1 is Y - 1,
    Y2 is Y + 1,
    ((checkLeft(Board,X1,Y,Player,0,Pieces1), Pieces1 > 0);
    (checkRight(Board,X2,Y,Player,0,Pieces2), Pieces2 > 0);
    (checkUp(Board,X,Y1,Player,0,Pieces3), Pieces3 > 0);
    (checkDown(Board,X,Y2,Player,0,Pieces4), Pieces4 > 0);
    (checkLeftUp(Board,X1,Y1,Player,0,Pieces5), Pieces5 > 0);
    (checkLeftDown(Board,X1,Y2,Player,0,Pieces6), Pieces6 > 0);
    (checkRightUp(Board,X2,Y1,Player,0,Pieces7), Pieces7 > 0);
    (checkRightDown(Board,X2,Y2,Player,0,Pieces8), Pieces8 > 0)).

% canPlay(+Board, +Player, -Points)
% checks if the player can make any plays    
canPlay(Board,Player,Points):-
    checkAllValidMoves(Board,Player,Points,0,0),
    length(Points,N),
    N1 is N - 1, !,
    N1 =\= 0.

% checkAllValidMoves(+Board, +Player, -Table, +Y, +X)
% returns an array with all possible plays
checkAllValidMoves(_,_,[],10,_).
checkAllValidMoves(Board, Player, [H|T], Y, X):-
    (validatePlay(Board,X,Y,Player) -> H = [X,Y],MOVE is 0 ; MOVE is 1),
    (compare(=,X,9) -> X1 is 0, Y1 is Y + 1; X1 is X + 1, Y1 is Y),
    (compare(=,MOVE,0) -> checkAllValidMoves(Board, Player, T, Y1, X1) ; (compare(=,Y1,10) -> checkAllValidMoves(Board, Player, T, Y1, X1); checkAllValidMoves(Board, Player, [H|T], Y1, X1) )).

% checkLeft(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces to the left of the pos
checkLeft(Board,X,Y,black,N,Pieces):-
    X >= 0,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkLeft(Board,X1,Y,black,N1,Pieces)),
    !.

checkLeft(Board,X,Y,white,N,Pieces):-
    X >= 0,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ; 
    N1 is N + 1,
    checkLeft(Board,X1,Y,white,N1,Pieces)),
    !.

checkLeft(_,_,_,_,0,0).

% checkRight(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces to the right of the pos        
checkRight(Board,X,Y,black,N,Pieces):-
    X =< 9,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRight(Board,X1,Y,black,N1,Pieces)),
    !.

checkRight(Board,X,Y,white,N,Pieces):-
    X =< 9,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRight(Board,X1,Y,white,N1,Pieces)),
    !.

checkRight(_,_,_,_,0,0).

% checkUp(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces to the above of the pos
checkUp(Board,X,Y,black,N,Pieces):-
    Y >= 0,
    Y1 is Y - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkUp(Board,X,Y1,black,N1,Pieces)),
    !.

checkUp(Board,X,Y,white,N,Pieces):-
    Y >= 0,
    Y1 is Y - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkUp(Board,X,Y1,white,N1,Pieces)),
    !.

checkUp(_,_,_,_,0,0). 

% checkDown(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces to the bellow of the pos
checkDown(Board,X,Y,black,N,Pieces):-
    Y =< 9,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkDown(Board,X,Y1,black,N1,Pieces)),
    !.

checkDown(Board,X,Y,white,N,Pieces):-
    Y =< 9,
    Y1 is Y + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkDown(Board,X,Y1,white,N1,Pieces)),
    !.

checkDown(_,_,_,_,0,0).

% checkLeftUp(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces diagonally up to the left of the pos
checkLeftUp(Board,X,Y,black,N,Pieces):-
    X >= 0,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkLeftUp(Board,X1,Y1,black,N1,Pieces)),
    !.

checkLeftUp(Board,X,Y,white,N,Pieces):-
    X >= 0,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkLeftUp(Board,X1,Y1,white,N1,Pieces)),
    !.

checkLeftUp(_,_,_,_,0,0). 

% checkLeftDown(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces diagonally down to the left of the pos
checkLeftDown(Board,X,Y,black,N,Pieces):-
    X >= 0,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkLeftDown(Board,X1,Y1,black,N1,Pieces)),
    !.

checkLeftDown(Board,X,Y,white,N,Pieces):-
    X >= 0,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X - 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkLeftDown(Board,X1,Y1,white,N1,Pieces)),
    !.

checkLeftDown(_,_,_,_,0,0).

% checkRightUp(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces diagonally up to the right of the pos
checkRightUp(Board,X,Y,black,N,Pieces):-
    X =< 9,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRightUp(Board,X1,Y1,black,N1,Pieces)),
    !.

checkRightUp(Board,X,Y,white,N,Pieces):-
    X =< 9,
    Y >= 0,
    Y1 is Y - 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRightUp(Board,X1,Y1,white,N1,Pieces)),
    !.

checkRightUp(_,_,_,_,0,0).

% checkRightUp(+Board,+X,+Y,+Player,+N,-Pieces)
% checks if play is possible by checking all pieces diagonally down to the right of the pos
checkRightDown(Board,X,Y,black,N,Pieces):-
    X =< 9,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, black) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRightDown(Board,X1,Y1,black,N1,Pieces)),
    !.

checkRightDown(Board,X,Y,white,N,Pieces):-
    X =< 9,
    Y =< 9,
    Y1 is Y + 1,
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    \+compare(=, Piece, none),
    \+compare(=, Piece, wall),
    \+compare(=, Piece, bonus),
    ((compare(=, Piece, white) ; compare(=, Piece, joker)) -> Pieces is N ;
    N1 is N + 1,
    checkRightDown(Board,X1,Y1,white,N1,Pieces)),
    !.

checkRightDown(_,_,_,_,0,0).

% game_over(+Board-,Winner,+Skips)
% returns winner of the game
game_over(Board, Winner, Skips):-
    isGameOver(Board,0,0,Skips),
    getBlackPlayerScore(Board, N1),
    getWhitePlayerScore(Board,N2),
    (N1 > N2 -> Winner = black ; Winner = white).

% isGameOver(Board,X,Y,Skips)
% checks if the game is over
isGameOver(_,_,10,_).
isGameOver(_,_,_,2).
isGameOver(Board,X,Y,Skips):-
    checkRowEmptyPlaces(Board,X,Y),
    Skips < 2,
    Y1 is Y + 1,
    isGameOver(Board,X,Y1,Skips).

% checkRowEmptyPlaces(+Board,+X,+Y)
% checks there is a cell empty in the row
checkRowEmptyPlaces(_,10,_).
checkRowEmptyPlaces(Board,X,Y):-
    getPiece(Y,X,Board,Piece),
    getRep(Piece, Rep),
    (compare(=, Rep, '.') -> fail),
    X1 is X + 1,
    checkRowEmptyPlaces(Board,X1,Y).

% getBlackPlayerScore(+Board,-N)
% checks there is a cell empty in the row
getBlackPlayerScore(Board,N):-
    getAllBlackRowsScores(Board,0,0,Scores),
    sumlist(Scores,CombScores),
    N = CombScores.

% getWhitePlayerScore(+Board,-N)
% checks there is a cell empty in the row
getWhitePlayerScore(Board,N):-
    getAllWhiteRowsScores(Board,0,0,Scores),
    sumlist(Scores,CombScores),
    N = CombScores.

getAllBlackRowsScores(_,_,10,_).
getAllBlackRowsScores(Board,X,Y,[H|T]):-
    Y1 is Y + 1,
    getBlackRowScore(X,Y,Board,RowScore),
    sumlist(RowScore,N),
    H = N,
    getAllBlackRowsScores(Board,X,Y1,T).

getAllWhiteRowsScores(_,_,10,_).
getAllWhiteRowsScores(Board,X,Y,[H|T]):-
    Y1 is Y + 1,
    getWhiteRowScore(X,Y,Board,RowScore),
    sumlist(RowScore,N),
    H = N,
    getAllWhiteRowsScores(Board,X,Y1,T).

getBlackRowScore(10,_,_,_).
getBlackRowScore(X,Y,Board,[H|T]):-
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=,Piece,black) -> H = 1 , getBlackRowScore(X1,Y,Board,T); H = 0, getBlackRowScore(X1,Y,Board,T)).

getWhiteRowScore(10,_,_,_).
getWhiteRowScore(X,Y,Board,[H|T]):-
    X1 is X + 1,
    getPiece(Y,X,Board,Piece),
    (compare(=,Piece,white) -> H = 1 , getWhiteRowScore(X1,Y,Board,T); H = 0, getWhiteRowScore(X1,Y,Board,T)).
    
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