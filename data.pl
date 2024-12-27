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


board(2, [
    [w, w, empty, empty, empty, empty, empty, empty],
    [b, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty]
]).


symbol(w, 'W').
symbol(b, 'B').
symbol(empty, '.').



% -----------------------------------------------
% 2. Players
% -----------------------------------------------

player(1, player1).
player(2, player2).

piece(player1, w).
piece(player2, b).

change_player(player1, player2).
change_player(player2, player1).
