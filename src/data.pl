% -----------------------------------------------
% Board
% -----------------------------------------------

% Initial board configuration
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

