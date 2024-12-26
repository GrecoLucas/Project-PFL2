menu :-
    write('========================='), nl,
    write('      STORM CLOUDS       '), nl,
    write('========================='), nl,
    write('1. Start Game'), nl,
    write('2. Rules'), nl,
    write('3. Exit'), nl,
    write('========================='), nl,
    write('Choose an option: '),
    read(Option),
    execute_option(Option).

game_B :-
    write('Black\'s turn'), nl,
    display_board,
    write('Choose an option: '), nl,
    write('1. Move'), nl,
    write('2. Capture Piece'), nl,
    write('3. Pass the turn'), nl,
    read(Option),
    execute_option_B(Option),
    game_W.

game_W :-
    write('White\'s turn'), nl,
    display_board,
    write('Choose an option: '), nl,
    write('1. Move'), nl,
    write('2. Capture Piece'), nl,
    write('3. Pass the turn'), nl,
    read(Option),
    execute_option_W(Option),
    game_B.

execute_option_W(1) :-
    write('Move'), nl.
execute_option_W(2) :-
    write('Capture Piece'), nl.
execute_option_W(3) :-
    write('Pass the turn'), nl.

execute_option_B(1) :-
    write('Move'), nl.
execute_option_B(2) :-
    write('Capture Piece'), nl.
execute_option_B(3) :-
    write('Pass the turn'), nl.

execute_option(1) :-
    start_game.
execute_option(2) :-
    show_rules, nl, menu.
execute_option(3) :-
    write('Thanks for playing Storm Clouds!'), nl.
execute_option(_) :-
    write('Invalid option, please try again.'), nl, menu.

start_game :-
    write('Starting the game...'), nl,
    display_board,
    write('Good luck!'), nl,
    game_B.

show_rules :-
    write('Storm Clouds Rules:'), nl,
    write('- The game is played on an 8x8 board.'), nl,
    write('- Each player begins with 12 pieces.'), nl,
    write('- Non-capturing moves (1 tile) - Black moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('- Non-capturing moves (1 tile) - White moves up, diagonal-up-left, right, diagonal-up-right.'), nl,
    write('- Capturing moves (multiple tiles) - Black moves down, diagonal-up-left, left, diagonal-down-left.'), nl,
    write('- Capturing moves (multiple tiles) - White moves down, diagonal-down-right, left, diagonal-down-left.'), nl,
    write('- The objective is to eliminate all opponent\'s pieces.'), nl,
    write('- If a player cannot move any piece, they must pass the turn.'), nl.

:- dynamic board/1.

% Initial board state
board([
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    [w, w, '.', '.', '.', '.', '.', '.'],
    ['.', '.', b, b, b, b, b, b],
    ['.', '.', b, b, b, b, b, b]
]).

% Display the board
display_board :-
    board(Board),
    display_board_aux(Board).

display_board_aux([]).
display_board_aux([Row | Rest]) :-
    display_row(Row),
    display_board_aux(Rest).

display_row([]) :-
    nl.
display_row([Piece | Rest]) :-
    write(Piece), write(' '),
    display_row(Rest).

% Update a specific cell on the board
% update_board(X, Y, NewPiece).
update_board(X, Y, NewPiece) :-
    board(Board),
    nth0(X, Board, Row, RestRows),
    nth0(Y, Row, _OldPiece, RestPieces),
    nth0(Y, NewRow, NewPiece, RestPieces),
    nth0(X, NewBoard, NewRow, RestRows),
    retract(board(Board)),
    asserta(board(NewBoard)).

% Test board update
test :-
    write('Initial board:'), nl,
    display_board,
    update_board(0, 0, '.'), % Replace the piece at position (0, 0) with '.'
    write('Board after update:'), nl,
    display_board.
