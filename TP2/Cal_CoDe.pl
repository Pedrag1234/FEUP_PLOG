:- use_module(library(clpfd)).
:- use_module(library(lists)).

%Team: Name-City-HomeMatches-AwayMatches
teams(['FCPorto'-'Porto'-0-0, 'Benfica'-'Lisboa'-0-0, 'Sporting'-'Lisboa'-0-0, 'Boavista'-'Porto'-0-0, 'SCBraga'-'Braga'-0-0, 'BSAD'-'Lisboa'-0-0,
       'GilVicente'-'Guimaraes'-0-0, 'Famalicao'-'VilaNovaDeFamalicao'-0-0, 'Farense'-'Faro'-0-0, 'Maritimo'-'Funchal'-0-0,
       'PacosDeFerreira'-'PacosDeFerreira'-0-0, 'Nacional'-'Funchal'-0-0, 'RioAve'-'VilaDoConde'-0-0, 'Tondela'-'Tondela'-0-0,
       'VitoriaDeGuimaraes'-'Guimaraes'-0-0, 'SantaClara'-'PontaDelgada'-0-0, 'Portimonense'-'Portimao'-0-0, 'Moreirense'-'MoreiraDeConegos'-0-0]).

run(Teams, Laps):-
    generateSeason(Teams, Laps, first, _, [], SeasonMatches),
    print(SeasonMatches).

% generateSeason(+Teams, +Laps, +LapGeneration, +LastLapMatches, +MatchesAcc, -SeasonMatches)
% Generates all the matches in a season, according to the teams + lap numbers provided.
generateSeason(_, 0, _, _, SeasonMatches, SeasonMatches).
generateSeason(Teams, Laps, first, LastLapMatches, MatchesAcc, SeasonMatches):-
    generateLap(Teams, LastLapMatches, NewTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, LastLapMatches, NewMatchesAcc),
    generateSeason(NewTeams, NewLaps, alternate, LastLapMatches, NewMatchesAcc, SeasonMatches).

generateSeason(Teams, Laps, alternate, LastLapMatches, MatchesAcc, SeasonMatches):-
    alternateMatchSides(LastLapMatches, [], AlternatedMatches),
    generateAlternateLap(Teams, AlternatedMatches, NewTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, AlternatedMatches, NewMatchesAcc),
    generateSeason(NewTeams, NewLaps, alternate, AlternatedMatches, NewMatchesAcc, SeasonMatches).

generateLap(Teams, LapMatches,NewLapMatches):-
    generateMatches(Teams,Matches),
    restrictSameLists(LapMatches,Matches),
    %generateAlternateLap(Teams,Matches,ReverseMatches),
    NewLapMatches = [LapMatches|Matches].
    %NewLapMatches = [ReverseMatches|NewLapMatches],  
    %write(NewLapMatches),nl.

generateAlternateLap(Teams, Matches, NewTeams):-
    % add restrictions
    addMatches(Teams, Matches, NewTeams).

generateMatches([],_).
generateMatches(Teams,[MatchesH|MatchesT]):-
    generateMatch(Teams,Home,Away),
    Home = HomeName-HomeCity-HHomeMatches-HAwayMatches,
    Away = AwayName-AwayCity-AHomeMatches-AAwayMatches,
    Match = HomeName-AwayName,
    removeTeams(Teams,Home,Away,[],NewTeams),
    MatchesH = Match,
    generateMatches(NewTeams , MatchesT).

generateMatch(Teams, Home,Away):-
    nth0(N1, Teams, Home),
    nth0(N2, Teams, Away),
    N1 \== N2.

alternateMatchSides([], AlternatedMatches, AlternatedMatches).
alternateMatchSides([Match|MatchT], MatchesAcc, AlternatedMatches):-
    Match = HomeTeam-AwayTeam,
    AlternateMatch = AwayTeam-HomeTeam,
    append(MatchesAcc, [AlternateMatch], NewMatchesAcc),
    alternateMatchSides(MatchT, NewMatchesAcc, AlternatedMatches).

addMatches(NewTeams, [], NewTeams).
addMatches(Teams, [Match|MatchT], NewTeams):-
    addMatch(Teams, Match, [], TempTeams),
    addMatches(TempTeams, MatchT, NewTeams).

addMatch([], _, TeamsAcc, TeamsAcc).
addMatch([Team|T], Match, TeamsAcc, NewTeams):-
    Match = HomeTeam-AwayTeam,
    Team = Name-City-HomeMatches-AwayMatches,
    ((Name = HomeTeam, NewHomeMatches is HomeMatches + 1, NewTeam = Name-City-NewHomeMatches-AwayMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (Name = AwayTeam, NewAwayMatches is AwayMatches + 1, NewTeam = Name-City-HomeMatches-NewAwayMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (addMatch(T,Match, [Team|TeamsAcc], NewTeams))).

removeTeams([],_,_,NewTeams,NewTeams).
removeTeams([TeamsH|TeamsT],Home,Away,RemAcc,NewTeams):-
    (( TeamsH = Home ; TeamsH = Away) -> removeTeams(TeamsT,Home,Away,RemAcc,NewTeams) ; removeTeams(TeamsT,Home,Away,[TeamsH|RemAcc],NewTeams)).

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
    