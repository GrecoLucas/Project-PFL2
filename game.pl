menu :-
    write('========================='), nl,
    write('      STORM CLOUDS       '), nl,
    write('========================='), nl,
    write('1. Iniciar jogo'), nl,
    write('2. Regras do jogo'), nl,
    write('3. Sair'), nl,
    write('========================='), nl,
    write('Escolha uma opcao: '),
    read(Opcao),
    executar_opcao(Opcao).

jogo_B :-
    write('Turno do Jogador Preto'), nl,
    exibir_tabuleiro,
    write('Escolha uma opcao: '), nl,
    write('1. Mover'), nl,
    write('2. Capturar Peça'), nl,
    write('3. Passar a vez'), nl,
    read(Opcao),
    executar_opcaoJ_B(Opcao),
    jogo_W.

jogo_W :-
    write('Turno do Jogador Branco'), nl,
    exibir_tabuleiro,
    write('Escolha uma opcao: '), nl,
    write('1. Mover'), nl,
    write('2. Capturar Peça'), nl,
    write('3. Passar a vez'), nl,
    read(Opcao),
    executar_opcaoJ_W(Opcao),
    jogo_B.

executar_opcaoJ_W(1) :-
    write('Mover'), nl.
executar_opcaoJ_W(2) :-
    write('Capturar Peça'), nl.
executar_opcaoJ_W(3) :-
    write('Passar a vez'), nl.

executar_opcaoJ_B(1) :-
    write('Mover'), nl.
executar_opcaoJ_B(2) :-
    write('Capturar Peça'), nl.
executar_opcaoJ_B(3) :-
    write('Passar a vez'), nl.

executar_opcao(1) :-
    iniciar_jogo.
executar_opcao(2) :-
    mostrar_regras, nl, menu.
executar_opcao(3) :-
    write('Obrigado por jogar Storm Clouds!'), nl.
executar_opcao(_) :-
    write('Opcao invalida, tente novamente.'), nl, menu.

iniciar_jogo :-
    write('Iniciando o jogo...'), nl,
    exibir_tabuleiro,
    write('Boa sorte!'), nl,
    jogo_B.

mostrar_regras :-
    write('Regras do Storm Clouds:'), nl,
    write('- O tabuleiro é um grid 8x8 (tabuleiro de damas).'), nl,
    write('- Cada jogador começa com 12 peças dispostas em 2 fileiras.'), nl,
    write('- Black move para o norte, nordeste, leste e sudeste sem capturar.'), nl,
    write('- White move para o leste, nordeste, norte e noroeste sem capturar.'), nl,
    write('- Ambos podem capturar em todas as outras direções, como uma rainha do xadrez.'), nl,
    write('- O objetivo é eliminar todas as peças do oponente.'), nl,
    write('- Se não houver movimento legal, o jogador pode passar a vez.'), nl,
    write('- O jogo não pode terminar em empate.'), nl.

% Declarar o tabuleiro como fato dinâmico
:- dynamic tabuleiro/1.

% Estado inicial do tabuleiro
tabuleiro([
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    ['.', '.', b, b, b, b, b, b],
    ['.', '.', b, b, b, b, b, b]
]).

% Exibe o tabuleiro na tela
exibir_tabuleiro :-
    tabuleiro(Tabuleiro),
    exibir_tabuleiro_aux(Tabuleiro).

exibir_tabuleiro_aux([]).
exibir_tabuleiro_aux([Linha | Resto]) :-
    exibir_linha(Linha),
    exibir_tabuleiro_aux(Resto).

exibir_linha([]) :-
    nl.
exibir_linha([Peca | Resto]) :-
    write(Peca), write(' '),
    exibir_linha(Resto).

% Atualizar uma célula específica do tabuleiro
% update_tabuleiro(X, Y, NovaPeca).
update_tabuleiro(X, Y, NovaPeca) :-
    tabuleiro(Tabuleiro),
    nth0(X, Tabuleiro, Linha, RestoLinhas),
    nth0(Y, Linha, _PecaAntiga, RestoPecas),
    nth0(Y, NovaLinha, NovaPeca, RestoPecas),
    nth0(X, NovoTabuleiro, NovaLinha, RestoLinhas),
    retract(tabuleiro(Tabuleiro)),
    asserta(tabuleiro(NovoTabuleiro)).

% Testar a atualização do tabuleiro
teste :-
    write('Tabuleiro inicial:'), nl,
    exibir_tabuleiro,
    update_tabuleiro(0, 0, '.'), % Substitui a peça na posição (0, 0) por '.'
    write('Tabuleiro após atualização:'), nl,
    exibir_tabuleiro.