:- use_module(library(clpfd)).
:- use_module(library(lists)).

%Team: Name-City-HomeMatches-AwayMatches
teams(['FCPorto'-'Porto'-0-0, 'Benfica'-'Lisboa'-0-0, 'Sporting'-'Lisboa'-0-0, 'SCBraga'-'Braga'-0-0, 'Boavista'-'Porto'-0-0, 'BSAD'-'Lisboa'-0-0,
       'GilVicente'-'Guimaraes'-0-0, 'Famalicao'-'VilaNovaDeFamalicao'-0-0, 'Farense'-'Faro'-0-0, 'Maritimo'-'Funchal'-0-0,
       'PacosDeFerreira'-'PacosDeFerreira'-0-0, 'Nacional'-'Funchal'-0-0, 'RioAve'-'VilaDoConde'-0-0, 'Tondela'-'Tondela'-0-0, 
       'VitoriaDeGuimaraes'-'Guimaraes'-0-0, 'SantaClara'-'PontaDelgada'-0-0, 'Portimonense'-'Portimao'-0-0, 'Moreirense'-'MoreiraDeConegos'-0-0]).

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
    generateSeason(Teams, Laps, first, _, [], SeasonMatches, _),
    length(Teams, Length),
    JornadaMatches is round(Length / 2),
    printMatches(SeasonMatches, JornadaMatches, 0, 0).

% generateSeason(+Teams, +Laps, +LapGeneration, +LastLapMatches, +MatchesAcc, -SeasonMatches, -NewTeams)
% Generates all the matches in a season, according to the teams + lap numbers provided.
generateSeason(NewTeams, 0, _, _, SeasonMatches, SeasonMatches, NewTeams).
generateSeason(Teams, Laps, first, LastLapMatches, MatchesAcc, SeasonMatches, NewTeams):-
    length(Teams,Size),
    LapJornadas is Size - 1,
    generateLap(Teams, LapJornadas, [], LastLapMatches, TempTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, LastLapMatches, NewMatchesAcc),
    generateSeason(TempTeams, NewLaps, alternate, LastLapMatches, NewMatchesAcc, SeasonMatches, NewTeams).

generateSeason(Teams, Laps, alternate, LastLapMatches, MatchesAcc, SeasonMatches, NewTeams):-
    generateAlternateLap(Teams, LastLapMatches, AlternatedMatches, TempTeams),
    NewLaps is Laps - 1,
    append(MatchesAcc, AlternatedMatches, NewMatchesAcc),
    generateSeason(TempTeams, NewLaps, alternate, AlternatedMatches, NewMatchesAcc, SeasonMatches, NewTeams).

% generateLap(+Teams, +LapJornadas, +MatchesAcc, -LapMatches, -NewTeams)
% Generates all the matches in a lap, depending on the number of teams in the league.
generateLap(Teams, 0,  LapMatches, LapMatches, Teams).
generateLap(Teams, LapJornadas, MatchesAcc, LapMatches, NewTeams):-
    validateMatches(Teams, MatchesAcc, Matches),
    addMatches(Teams,Matches,TempTeams),
    append(MatchesAcc,Matches,NewMatchesAcc),
    NewLapJornadas is LapJornadas - 1,
    generateLap(TempTeams, NewLapJornadas, NewMatchesAcc, LapMatches, NewTeams).

% generateAlternateLap(+Teams, +LastLapMatches, -AlternatedMatches, -NewTeams)
% Generates all the matches in a lap that occurred after the first lap, repeating the previous lap with changed sides on the matches.
generateAlternateLap(Teams, LastLapMatches, AlternatedMatches, NewTeams):-
    alternateMatchSides(LastLapMatches, [], AlternatedMatches),
    addMatches(Teams, AlternatedMatches, NewTeams).

% validateMatches(+Teams, +PreviousMatches, -Matches)
% Returns a possible weekly set of matches, respecting the restrictions put in place.
validateMatches(Teams, PreviousMatches, Matches):-
    length(Teams,Size),
    domain([HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI],1,Size),
    domain([AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI],1,Size),
    all_distinct([HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI,AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI]),
    %countImportantMatches(Teams, [HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI], 0, Count),
    %Count #= 2,
    checkPreviousMatches(Teams, PreviousMatches, [HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI], [AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI]),
    generateMatches(Teams,[HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI],[AwayA,AwayB,AwayC,AwayD,AwayE,AwayF,AwayG,AwayH,AwayI],[],Matches).
    %checkDoubleHomeMatches(Teams, [HomeA,HomeB,HomeC,HomeD,HomeE,HomeF,HomeG,HomeH,HomeI], []).

% countImportantMatches(+Teams, +HomeTeams, +CountAcc, -Count)
% Counts the amount of matches that occur on the home of a 'big' team
%countImportantMatches(_, [], Count, Count).
%countImportantMatches(Teams, [HomeTeamsH|HomeTeamsT], CountAcc, Count):-
%    nth1(HomeTeamsH, Teams, Home),
%    Home = HomeName-_-_,
%    (( HomeName = 'FCPorto' ; HomeName = 'Benfica' ; HomeName = 'Sporting' ; HomeName = 'SCBraga' ) 
%    -> ( NewCountAcc is CountAcc + 1, countImportantMatches(Teams, HomeTeamsT, NewCountAcc, Count) ); countImportantMatches(Teams, HomeTeamsT, CountAcc, Count)).

% checkDoubleHomeMatches(+Teams, +HomeTeams, ?Cities)
% Checks to see if there is more than one game being played on the same city, on a given weekly set of teams playing at home.
%checkDoubleHomeMatches(_, [], Cities):-
%    length(Cities, FirstLength),
%    remove_dups(Cities, Pruned),
%    length(Pruned, SecondLength),
%    FirstLength = SecondLength.
    
%checkDoubleHomeMatches(Teams, [HomeTeamsH|HomeTeamsT], Cities):-
%    nth1(HomeTeamsH, Teams, Home),
%    Home = _-City-_-_,
%    checkDoubleHomeMatches(Teams, HomeTeamsT, [City|Cities]).
    
% checkPreviousMatches(+Teams, +PreviousMatches, +HomeTeams, +AwayTeams)
% Checks to see if the chosen set of teams respects the restrictions put in place (alternating sides every weekly set of games + not playing a match that has been already played before).
checkPreviousMatches(_, _, [], []).
checkPreviousMatches(Teams, PreviousMatches, [HomeTeamsH|HomeTeamsT], [AwayTeamsH|AwayTeamsT]):-
    nth1(HomeTeamsH, Teams, Home),
    nth1(AwayTeamsH, Teams, Away),
    Home = HomeName-_-HHomeMatches-HAwayMatches,
    Away = AwayName-_-AHomeMatches-AAwayMatches,
    HHomeMatches =< HAwayMatches,
    AAwayMatches =< AHomeMatches,
    checkPreviousOpponents(PreviousMatches, HomeName, AwayName),
    checkPreviousMatches(Teams, PreviousMatches, HomeTeamsT, AwayTeamsT).

% checkPreviousOpponents(+PreviousMatches, +HomeName, +AwayName)
% Checks to see if the two given teams have already played in the past matches.
checkPreviousOpponents([], _, _).
checkPreviousOpponents([Match|PreviousMatchesT], HomeName, AwayName):-
    Match \== HomeName-AwayName, Match \== AwayName-HomeName,
    checkPreviousOpponents(PreviousMatchesT, HomeName, AwayName).

% generateMatches(+Teams, +HomeTeams, +AwayTeams, +MatchesAcc, -Matches)
% Generates a weekly set of matches, with the given teams staying at home/going away.
generateMatches(_,[],[],Matches,Matches).
generateMatches(Teams,[HomeTeamsH|HomeTeamsT],[AwayTeamsH|AwayTeamsT],MatchesAcc,Matches):-
    nth1(HomeTeamsH, Teams, Home),
    nth1(AwayTeamsH, Teams, Away),
    Home = HomeName-_-_-_,
    Away = AwayName-_-_-_,
    Match = HomeName-AwayName,
    append(MatchesAcc,[Match],NewMatchesAcc),
    generateMatches(Teams,HomeTeamsT,AwayTeamsT,NewMatchesAcc,Matches).

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
    Team = Name-City-HomeMatches-AwayMatches,
    ((Name = HomeTeam, NewHomeMatches is HomeMatches + 1, NewTeam = Name-City-NewHomeMatches-AwayMatches, append(TeamsAcc, [NewTeam], NewTeamsAcc), addMatch(T, Match, NewTeamsAcc, NewTeams));
    (Name = AwayTeam, NewAwayMatches is AwayMatches + 1, NewTeam = Name-City-HomeMatches-NewAwayMatches, append(TeamsAcc, [NewTeam], NewTeamsAcc), addMatch(T, Match, NewTeamsAcc, NewTeams));
    (append(TeamsAcc, [Team], NewTeamsAcc), addMatch(T,Match, NewTeamsAcc, NewTeams))).