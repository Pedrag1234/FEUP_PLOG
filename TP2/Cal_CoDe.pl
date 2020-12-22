:- use_module(library(clpfd)).

equipa('FC Porto','Porto').
equipa('Benfica','Lisboa').
equipa('Sporting','Lisboa').
equipa('Boavista','Porto').
equipa('SC Braga','Braga').
equipa('B SAD','Lisboa').
equipa('Gil Vicente','Guimarães').
equipa('Famalicao','Vila Nova de Famalicão').
equipa('Farense','Faro').
equipa('Maritimo','Funchal').
equipa('Pacos de Ferreira','Paços de Ferreira').
equipa('Nacional','Funchal').
equipa('Rio Ave','Vila do Conde').
equipa('Tondela','Tondela').
equipa('Vitória de Guimarães','Guimarães').
equipa('Santa Clara','Ponta Delgada').
equipa('Portimonense','Portimão').
equipa('Moreirense','Moreira de Cónegos').


printMatch(Equipa1,Equipa2,N):-
    equipa(Equipa1,Nome1),
    equipa(Equipa2,Nome2),
    write('Match ') , write(N) , nl,
    write('Localidade : ') , write(Equipa1) , nl,
    write('Em casa : ') , write(Equipa1) , nl,
    write('Fora : ') , write(Equipa2) , nl.

Cal_CoDe:-
    fail.
    