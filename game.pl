% Onde o jogo começa chamada menu.
% GameState representa o estado atual do jogo.
% GameState é composto por (Current_board, Current_player).

% Tabuleiro inicial
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

% Variáveis do board
item(b, 'Black').
item(w, 'White').
item(empty, 'Empty').

% Variáveis do player
player(1, player1).
player(2, player2).

% Variáveis da peça
piece(player1, w).
piece(player2, b).

% Trocar de player
change_player(player1, player2).
change_player(player2, player1).

% Selecionar o tabuleiro
get_board(N, Board) :-
    board(N, Board).

% Display do item no tabuleiro
display_item(Item) :-
    item(Item, C),
    write(C), write(' ').

% Display de uma linha do tabuleiro
display_row([]) :- nl.
display_row([Item|RemainingItems]) :-
    display_item(Item),
    display_row(RemainingItems).

% Display do tabuleiro
display_board([]).
display_board([Row|RemainingRows]) :-
    display_row(Row),
    display_board(RemainingRows).

% Display do jogador atual
display_player(Player) :-
    player(_, PlayerName),
    write('Current Player: '), write(PlayerName), nl.

% Obter uma peça no tabuleiro
get_piece(Board, X, Y, Piece) :-
    nth0(Y, Board, Line),
    nth0(X, Line, Piece).

% Verificar se um movimento é válido
valid_move(Board, X-Y) :- 
    get_piece(Board, X, Y, empty).

% Escolher uma jogada válida
choose_move(Board, X-Y) :-
    length(Board, N),
    Max is N - 1,
    repeat,
    get_number(0, Max, 'Coordinate X', X), 
    get_number(0, Max, 'Coordinate Y', Y),
    valid_move(Board, X-Y), 
    !.

% Colocar uma peça no tabuleiro
put_piece(Board, X-Y, Piece, NewBoard) :-
    nth0(Y, Board, Line, RestLines),
    nth0(X, Line, _, RestItems),
    nth0(X, NewLine, Piece, RestItems),
    nth0(Y, NewBoard, NewLine, RestLines).

% Get a number from the user within a range
get_number(Min, Max, Prompt, Number) :-
    repeat,
    write(Prompt), write(': '),
    read(Number),
    integer(Number),
    Number >= Min,
    Number =< Max,
    !.

% Menu inicial
play :-
    get_board(1, Board),
    game_loop((Board, player1)).

% Loop do jogo
game_loop((Current_board, Current_player)) :-
    display_board(Current_board),
    display_player(Current_player),
    choose_move(Current_board, X-Y),
    piece(Current_player, Piece),
    put_piece(Current_board, X-Y, Piece, New_board),
    change_player(Current_player, New_player),
    game_loop((New_board, New_player)).



menu :-
    write('Welcome to Storm Clouds!'), nl,
    write('1. Play'), nl,
    write('2. Rules'), nl,
    write('3. Exit'), nl,
    read(Choice),
    menu_choice(Choice).

menu_choice(1) :- play.
menu_choice(2) :- rules, menu.
menu_choice(3) :- write('Goodbye!'), nl.


rules :-
    write('Storm Clouds Rules:'), nl,
    write('- The game is played on an 8x8 board.'), nl,
    write('- Each player begins with 12 pieces.'), nl,
    write('- Non-capturing moves (1 tile) - Black moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('- Non-capturing moves (1 tile) - White moves up, diagonal-up-left, right, diagonal-up-right.'), nl,
    write('- Capturing moves (multiple tiles) - Black moves down, diagonal-up-left, left, diagonal-down-left.'), nl,
    write('- Capturing moves (multiple tiles) - White moves down, diagonal-down-right, left, diagonal-down-left.'), nl,
    write('- The objective is to eliminate all opponent\'s pieces.'), nl,
    write('- If a player cannot move any piece, they must pass the turn.'), nl.

