:- use_module(library(clpfd)).

%equipa('FC Porto','Porto').
%equipa('Benfica','Lisboa').
%equipa('Sporting','Lisboa').
%equipa('Boavista','Porto').
%equipa('SC Braga','Braga').
%equipa('B SAD','Lisboa').
%equipa('Gil Vicente','Guimaraes').
%equipa('Famalicao','Vila Nova de Famalicao').
%equipa('Farense','Faro').
%equipa('Maritimo','Funchal').
%equipa('Pacos de Ferreira','Pacos de Ferreira').
%equipa('Nacional','Funchal').
%equipa('Rio Ave','Vila do Conde').
%equipa('Tondela','Tondela').
%equipa('Vitoria de Guimaraes','Guimaraes').
%equipa('Santa Clara','Ponta Delgada').
%equipa('Portimonense','Portimao').
%equipa('Moreirense','Moreira de Conegos').

%Team: Name-City-HomeGames-VisitorGames
teams(['FCPorto'-'Porto'-0-0, 'Benfica'-'Lisboa'-0-0, 'Sporting'-'Lisboa'-0-0, 'Boavista'-'Porto'-0-0, 'SCBraga'-'Braga'-0-0, 'BSAD'-'Lisboa'-0-0,
       'GilVicente'-'Guimaraes'-0-0, 'Famalicao'-'VilaNovaDeFamalicao'-0-0, 'Farense'-'Faro'-0-0, 'Maritimo'-'Funchal'-0-0,
       'PacosDeFerreira'-'PacosDeFerreira'-0-0, 'Nacional'-'Funchal'-0-0, 'RioAve'-'VilaDoConde'-0-0, 'Tondela'-'Tondela'-0-0,
       'VitoriaDeGuimaraes'-'Guimaraes'-0-0, 'SantaClara'-'PontaDelgada'-0-0, 'Portimonense'-'Portimao'-0-0, 'Moreirense'-'MoreiraDeConegos'-0-0]).

%defineTeams(Teams):-
%    findall([Name-Location], equipa(Name,Location), Teams).

generateLap(Teams, ['FCPorto'-'Benfica'], Teams).

generateSeason(Teams):-
    generateLap(Teams, Games, NewTeams),
    generateAlternateLap(NewTeams, Games, NewTeams2),
    print(NewTeams2).

addGame([], _, TeamsAcc, TeamsAcc).
addGame([Team|T], Game, TeamsAcc, NewTeams):-
    Game = TeamHome-TeamVisitor,
    Team = Name-City-HomeGames-VisitorGames,
    ((Name = TeamHome, NewHomeGames is HomeGames + 1, NewTeam = Name-City-NewHomeGames-VisitorGames, addGame(T, Game, [NewTeam|TeamsAcc], NewTeams));
    (Name = TeamVisitor, NewVisitorGames is VisitorGames + 1, NewTeam = Name-City-HomeGames-NewVisitorGames, addGame(T, Game, [NewTeam|TeamsAcc], NewTeams));
    (addGame(T, Game, [Team|TeamsAcc], NewTeams))).

generateAlternateLap(NewTeams, [], NewTeams).
generateAlternateLap(Teams, [Game|T], NewTeams):-
        Game = TeamHome-TeamVisitor,
        AlternateGame = TeamVisitor-TeamHome,
        addGame(Teams, AlternateGame, [], TempTeams),!,
        generateAlternateLap(TempTeams, T, NewTeams).
%generateMatches(LapMatches):-.

%generateMatch(LapMatches):-.

checkValidMatch([],_):-!.
checkValidMatch([[M1-M2-L1]|T],[Team1-Team2-Location]):-
    M1 \== Team1,
    M2 \== Team2,
    checkValidMatch(T,[Team1-Team2-Location]).

restrictSameLists([],[]):-!.
restrictSameLists([H1|T1],[H2|T2]):-
    A #= B,
    restrictSameLists(T1,T2).

printMatch(Equipa1,Equipa2,N):-
    equipa(Equipa1,Nome1),
    equipa(Equipa2,Nome2),
    write('Match ') , write(N) , nl,
    write('Localidade : ') , write(Equipa1) , nl,
    write('Em casa : ') , write(Equipa1) , nl,
    write('Fora : ') , write(Equipa2) , nl.

cal_CoDe:-
    fail.
    