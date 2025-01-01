% -----------------------------------------------
% Dynamic Predicates
% -----------------------------------------------

% board/2 is dynamic because the board can change during the game (Bots, Players, etc.)
:- dynamic board/2.


% -----------------------------------------------
% Board
% -----------------------------------------------

% Tabuleiro jogável, onde w = white, b = black, empty = vazio, e o jogo acontece
board(1, [
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [empty, empty, b, b, b, b, b, b],
    [empty, empty, b, b, b, b, b, b]
]).

% Tabuleiro inicial, onde é usado para reiniciar o jogo
initial_board([
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [w, w, empty, empty, empty, empty, empty, empty],
    [empty, empty, b, b, b, b, b, b],
    [empty, empty, b, b, b, b, b, b]
]).


% -----------------------------------------------
% Board Symbols
% -----------------------------------------------

% Converte peças para símbolos
symbol(w, 'W').
symbol(b, 'B').
symbol(empty, ' ').


% -----------------------------------------------
% Players
% -----------------------------------------------

% Jogadores
player(1, player1). % Branco
player(2, player2). % Preto

% Peça de cada jogador
piece(player1, w).
piece(player2, b).

% -----------------------------------------------
% Troca de Jogador
% -----------------------------------------------

change_player(player1, player2).
change_player(player2, player1).
