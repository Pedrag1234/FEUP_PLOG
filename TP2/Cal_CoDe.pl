:- use_module(library(clpfd)).
:- use_module(library(lists)).

%Team: Name-City-HomeMatches-VisitorMatches
teams(['FCPorto'-'Porto'-0-0, 'Benfica'-'Lisboa'-0-0, 'Sporting'-'Lisboa'-0-0, 'Boavista'-'Porto'-0-0, 'SCBraga'-'Braga'-0-0, 'BSAD'-'Lisboa'-0-0,
       'GilVicente'-'Guimaraes'-0-0, 'Famalicao'-'VilaNovaDeFamalicao'-0-0, 'Farense'-'Faro'-0-0, 'Maritimo'-'Funchal'-0-0,
       'PacosDeFerreira'-'PacosDeFerreira'-0-0, 'Nacional'-'Funchal'-0-0, 'RioAve'-'VilaDoConde'-0-0, 'Tondela'-'Tondela'-0-0,
       'VitoriaDeGuimaraes'-'Guimaraes'-0-0, 'SantaClara'-'PontaDelgada'-0-0, 'Portimonense'-'Portimao'-0-0, 'Moreirense'-'MoreiraDeConegos'-0-0]).


generateLap(Teams, LapMatches,NewLapMatches):-
    generateMatches(Teams,Matches),
    restrictSameLists(LapMatches,Matches),
    %generateAlternateLap(Teams,Matches,ReverseMatches),
    NewLapMatches = [LapMatches|Matches],
    %NewLapMatches = [ReverseMatches|NewLapMatches],  
    write(NewLapMatches),nl.

generateSeason(Teams):-
    generateLap(Teams, Matches, NewTeams),
    generateAlternateLap(NewTeams, Matches, NewTeams2),
    print(NewTeams2).

addMatch([], _, TeamsAcc, TeamsAcc).
addMatch([Team|T], Match, TeamsAcc, NewTeams):-
    Match = TeamHome-TeamVisitor,
    Team = Name-City-HomeMatches-VisitorMatches,
    ((Name = TeamHome, NewHomeMatches is HomeMatches + 1, NewTeam = Name-City-NewHomeMatches-VisitorMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (Name = TeamVisitor, NewVisitorMatches is VisitorMatches + 1, NewTeam = Name-City-HomeMatches-NewVisitorMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (addMatch(T,Match, [Team|TeamsAcc], NewTeams))).

generateAlternateLap(NewTeams, [], NewTeams).
generateAlternateLap(Teams, [Match|T], NewTeams):-
        Match = TeamHome-TeamVisitor,
        AlternateMatch = TeamVisitor-TeamHome,
        addMatch(Teams, AlternateMatch, [], TempTeams),
        generateAlternateLap(TempTeams, T, NewTeams).


generateMatches([],_).
generateMatches(Teams,[MatchesH|MatchesT]):-
    generateMatch(Teams,Home,Away),
    Home = HomeName-HomeCity-HHomeMatches-HAwayMatches,
    Away = AwayName-AwayCity-AHomeMatches-AAwayMatches,
    Match = HomeName-AwayName,
    removeTeams(Teams,Home,Away,[],NewTeams),
    MatchesH = Match,
    generateMatches(NewTeams , MatchesT).

removeTeams([],_,_,NewTeams,NewTeams).
removeTeams([TeamsH|TeamsT],Home,Away,RemAcc,NewTeams):-
    (( TeamsH = Home ; TeamsH = Away) -> removeTeams(TeamsT,Home,Away,RemAcc,NewTeams) ; removeTeams(TeamsT,Home,Away,[TeamsH|RemAcc],NewTeams)).


generateMatch(Teams, Home,Away):-
    nth0(N1, Teams, Home),
    nth0(N2, Teams, Away),
    N1 \== N2.


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
    