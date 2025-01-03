% -----------------------------------------------
% Dynamic Predicates
% -----------------------------------------------

% board/2 is dynamic because the board can change during the game (Bots, Players, etc.)
:- dynamic board/2.


% -----------------------------------------------
% Board
% -----------------------------------------------

% Actual board, where the game is played
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

% Initial board (Immutable board, template to reset the board)
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

symbol(w, 'W').
symbol(b, 'B').
symbol(empty, ' ').


% -----------------------------------------------
% Players
% -----------------------------------------------

player(1, player1). % White
player(2, player2). % Black

piece(player1, w).
piece(player2, b).

change_player(player1, player2).
change_player(player2, player1).

