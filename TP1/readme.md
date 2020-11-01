# Mapello 

![Mapello](https://nestorgames.com/gameimages/mapello_mid.jpg)

## Turma 1 Grupo 5

### Trabalho realizado por:

| Nome          | Número    |
| --------------|-----------|
| Pedro Miguel Oliveira Azevedo | 201603816 |
| Diogo Ferreira de Sousa | 201706409 |


## Descrição:

O mapello é uma variante do reversi no qual é adicionado algumas peças extras em comparação com o original, sendo elas as Walls (peças que servem como as paredes do tabuleiro e não podem ser colocadas peças em cima), Bonuses (peças que dão mais pontos a quem os captura) e Jokers (peças normais que pertencem ao jogador atual). O objetivo do jogo é terminar o jogo com o maior número de pontos (pontos = peças + pontos bonus) isto faz-se capturando peças inimigas, tornando as da cor do jogador.

### Regras

- Apenas pode colocar uma peça por turno
- No início do jogo, o tabuleiro começa com 4 peças no centro (Ex.: B W | W B)
- Só pode colocar a peça se consegue capturar peças inimigas (Ex.:B W W _)
- Se não conseguir colocar nenhuma peça passa a jogada para o próximo jogador.
- Se os 2 jogadores passarem consecutivos o jogo acaba e ganha quem tiver mais pontos.
- O jogo acaba quando não houver mais jogadas possíveis (Ex.: Tabuleiro completamente cheio).

### Links

[Nestor](https://nestorgames.com/#mapello_detail)

## Representação de Interna do estado do Jogo

O estado do tabuleiro será guardado numa lista de listas (10 x 10) em qual cada posição vai guardar a peça que está aí. O jogador apenas ira guardar a sua pontuação tendo em conta que esta diminui ou aumenta de acordo com as peças que captura e perde.

### Peças

- "W" : peças brancas
- "B" : peças pretas
- "." : espaço vazio
- "G" : parede
- "J" : joker
- "P" : bonus
