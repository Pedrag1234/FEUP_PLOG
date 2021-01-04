:- use_module(library(clpfd)).
:- use_module(library(lists)).

%Team: Name-City-HomeMatches-AwayMatches
teams([1-'FCPorto'-'Porto'-0-0, 2-'Benfica'-'Lisboa'-0-0, 3-'Sporting'-'Lisboa'-0-0, 4-'Boavista'-'Porto'-0-0, 5-'SCBraga'-'Braga'-0-0, 6-'BSAD'-'Lisboa'-0-0,
       7-'GilVicente'-'Guimaraes'-0-0, 8-'Famalicao'-'VilaNovaDeFamalicao'-0-0, 9-'Farense'-'Faro'-0-0, 10-'Maritimo'-'Funchal'-0-0,
       11-'PacosDeFerreira'-'PacosDeFerreira'-0-0, 12-'Nacional'-'Funchal'-0-0, 13-'RioAve'-'VilaDoConde'-0-0, 14-'Tondela'-'Tondela'-0-0,
       15-'VitoriaDeGuimaraes'-'Guimaraes'-0-0, 16-'SantaClara'-'PontaDelgada'-0-0, 17-'Portimonense'-'Portimao'-0-0, 18-'Moreirense'-'MoreiraDeConegos'-0-0]).

% printMatches(+Matches, +JornadaMatches, +CurrentMatch, +Jornada)
% Writes the list of matches on the screen, grouped by each weekly set of games.
printMatches([], _, _, _).
printMatches(Matches, JornadaMatches, JornadaMatches, Jornada):-
    NewJornada is Jornada + 1,
    nl,
    printMatches(Matches, JornadaMatches, 0, NewJornada).
printMatches([Match|MatchT], JornadaMatches, 0, Jornada):-
    JornadaWrite is Jornada + 1,
    write('Jornada '), write(JornadaWrite), write(': | '), write(Match), write(' | '),
    printMatches(MatchT, JornadaMatches, 1, Jornada).
printMatches([Match|MatchT], JornadaMatches, CurrentMatch, Jornada):-
    write(Match), write(' | '),
    NewCurrentMatch is CurrentMatch + 1,
    printMatches(MatchT, JornadaMatches, NewCurrentMatch, Jornada).

% run(+Teams, +Laps)
% Starts the program, generating a football season calendar.
run(Teams, Laps):-
    generateSeason(Teams, Laps, first, _, [], SeasonMatches,NewTeams),
    length(Teams, Length),
    JornadaMatches is round(Length / 2),
    printMatches(SeasonMatches, JornadaMatches, 0, 0),
    print(NewTeams),nl.

% generateSeason(+Teams, +Laps, +LapGeneration, +LastLapMatches, +MatchesAcc, -SeasonMatches)
% Generates all the matches in a season, according to the teams + lap numbers provided.
generateSeason(NewTeams, 0, _, _, SeasonMatches, SeasonMatches,NewTeams).
generateSeason(Teams, Laps, first, LastLapMatches, MatchesAcc, SeasonMatches,NewTeams):-
    length(Teams,Size),
    LapJornadas is Size - 1,
    generateLap(Teams, LapJornadas, [], LastLapMatches, TempTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, LastLapMatches, NewMatchesAcc),
    generateSeason(TempTeams, NewLaps, alternate, LastLapMatches, NewMatchesAcc, SeasonMatches,NewTeams).

generateSeason(Teams, Laps, alternate, LastLapMatches, MatchesAcc, SeasonMatches,NewTeams):-
    alternateMatchSides(LastLapMatches, [], AlternatedMatches),
    generateAlternateLap(Teams, AlternatedMatches, TempTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, AlternatedMatches, NewMatchesAcc),
    generateSeason(TempTeams, NewLaps, alternate, AlternatedMatches, NewMatchesAcc, SeasonMatches,NewTeams).

generateLap(Teams, 0,  LapMatches, LapMatches, Teams).
generateLap(Teams, LapJornadas, MatchesAcc, LapMatches, NewTeams):-
    validateMatches(Teams,Matches),
    addMatches(Teams,Matches,TempTeams),
    append(MatchesAcc,Matches,NewMatchesAcc),
    NewLapJornadas is LapJornadas - 1,
    generateLap(TempTeams, NewLapJornadas, NewMatchesAcc, LapMatches, NewTeams).
    %restrictSameLists(LapMatches,Matches),
    %NewLapMatches = [LapMatches|Matches].

generateAlternateLap(Teams, Matches, NewTeams):-
    % add restrictions
    addMatches(Teams, Matches, NewTeams).

countImportantMatches([], Count, Count).
countImportantMatches([MatchesH|MatchesT], CountAcc, Count):-
    MatchesH = HomeName-AwayName,
    (( HomeName = 'FCPorto' ; HomeName = 'Benfica' ; HomeName = 'Sporting' ; HomeName = 'SCBraga' ) -> ( NewCountAcc is CountAcc + 1, countImportantMatches(MatchesT, NewCountAcc, Count) ); countImportantMatches(MatchesT, CountAcc, Count)).

validateMatches(Teams,Matches):-
    length(Teams,Size),
    domain([HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI],1,Size),
    domain([AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI],1,Size),
    all_distinct([HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI,AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI]),
    generateMatches(Teams,[HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI],[AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI],[],Matches),
    countImportantMatches(Matches, 0, Count),
    write(Count), nl,
    Count #> 1.

generateMatches(_,[],[],Matches,Matches).
generateMatches(Teams,[HomeTeamsH|HomeTeamsT],[AwayTeamsH|AwayTeamsT],MatchesAcc,Matches):-
    nth1(HomeTeamsH, Teams, Home),
    nth1(AwayTeamsH, Teams, Away),
    Home = HomeID-HomeName-HomeCity-HHomeMatches-HAwayMatches,
    Away = AwayID-AwayName-AwayCity-AHomeMatches-AAwayMatches,
    Match = HomeName-AwayName,
    append(MatchesAcc,[Match],NewMatchesAcc),
    generateMatches(Teams,HomeTeamsT,AwayTeamsT,NewMatchesAcc,Matches).

% generateMatch(+Teams, -Home, -Away)
% Creates a match between two different teams.
generateMatch(Teams, Home, Away):-
    nth0(N1, Teams, Home),
    nth0(N2, Teams, Away),
    N1 \== N2.

% alternateMatchSides(+Matches, +MatchesAcc, -AlternatedMatches)
% With a given list of matches, returns another list with the same matches but with the team sides reversed.
alternateMatchSides([], AlternatedMatches, AlternatedMatches).
alternateMatchSides([Match|MatchT], MatchesAcc, AlternatedMatches):-
    Match = HomeTeam-AwayTeam,
    AlternateMatch = AwayTeam-HomeTeam,
    append(MatchesAcc, [AlternateMatch], NewMatchesAcc),
    alternateMatchSides(MatchT, NewMatchesAcc, AlternatedMatches).

% addMatches(+Teams, +Matches, -NewTeams)
% Saves the given list of matches' occurrence on each team's record.
addMatches(NewTeams, [], NewTeams).
addMatches(Teams, [Match|MatchT], NewTeams):-
    addMatch(Teams, Match, [], TempTeams),
    addMatches(TempTeams, MatchT, NewTeams).

% addMatch(+Teams, +Match, +TeamsAcc, -NewTeams)
% Searches for the teams that participated in a given match, and saves the given match on their record.
addMatch([], _, TeamsAcc, TeamsAcc).
addMatch([Team|T], Match, TeamsAcc, NewTeams):-
    Match = HomeTeam-AwayTeam,
    Team = ID-Name-City-HomeMatches-AwayMatches,
    ((Name = HomeTeam, NewHomeMatches is HomeMatches + 1, NewTeam = ID-Name-City-NewHomeMatches-AwayMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (Name = AwayTeam, NewAwayMatches is AwayMatches + 1, NewTeam = ID-Name-City-HomeMatches-NewAwayMatches, addMatch(T, Match, [NewTeam|TeamsAcc], NewTeams));
    (addMatch(T,Match, [Team|TeamsAcc], NewTeams))).

% removeTeams(+Teams, +Match, +TeamsAcc, -NewTeams)
% Searches for the teams that participated in a given match, and saves the given match on their record.
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
    