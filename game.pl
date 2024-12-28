% -----------------------------------------------
% Data
% -----------------------------------------------
% -----------------------------------------------
% Board
% -----------------------------------------------
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

% Test board
board(2, [
    [empty, empty, empty, empty, empty, empty, empty, w],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [empty, empty, empty, empty, empty, empty, empty, empty],
    [b, empty, empty, empty, empty, empty, empty, empty]
]).



% -----------------------------------------------
% Board Symbols
% -----------------------------------------------

symbol(w, 'W').
symbol(b, 'B').
symbol(empty, '.').


% -----------------------------------------------
% Players
% -----------------------------------------------

player(1, player1).
player(2, player2).

piece(player1, w).
piece(player2, b).

change_player(player1, player2).
change_player(player2, player1).


% -----------------------------------------------
% Movement Rules
% -----------------------------------------------
% For White player (player1)
valid_move(Board, X-Y, Nx-Ny, player1) :- 
    get_piece(Board, X, Y, w),
    (
      % Non-capturing moves (1 tile)
      (Nx is X, Ny is Y - 1)    % Up
    ; (Nx is X + 1, Ny is Y - 1)  % Diagonal up-right
    ; (Nx is X + 1, Ny is Y)      % Right
    ; (Nx is X + 1, Ny is Y + 1)  % Diagonal down-right

      % Capturing moves (multiple tiles)
    ; (Nx > X, Ny is Y + (Nx - X))   % Down
    ; (Nx < X, Ny is Y + (X - Nx))   % Diagonal down-left
    ; (Nx < X, Ny is Y)              % Left
    ; (Nx > X, Ny is Y - (Nx - X))   % Diagonal up-left
    ),
    get_piece(Board, Nx, Ny, empty) ; get_piece(Board, Nx, Ny, b).

% For Black player (player2)
valid_move(Board, X-Y, Nx-Ny, player2) :- 
    get_piece(Board, X, Y, b),
    (
      % Non-capturing moves (1 tile)
      (Nx is X, Ny is Y - 1)    % Up
    ; (Nx is X + 1, Ny is Y - 1)  % Diagonal up-right
    ; (Nx is X - 1, Ny is Y - 1)  % Diagonal up-left
    ; (Nx is X + 1, Ny is Y)      % Right

      % Capturing moves (multiple tiles)
    ; (Nx > X, Ny is Y + (Nx - X))   % Down
    ; (Nx < X, Ny is Y)              % Left
    ; (Nx < X, Ny is Y + (X - Nx))   % Diagonal down-left
    ; (Nx > X, Ny is Y + (Nx - X))   % Diagonal down-right
    ),
    get_piece(Board, Nx, Ny, empty) ; get_piece(Board, Nx, Ny, w).


% -----------------------------------------------
% Display Board
% -----------------------------------------------

% Display the entire board with column/row labels
display_board(Board) :-
    write('    0 1 2 3 4 5 6 7'), nl,
    display_rows(Board, 0).

% Recursively print each row with index
display_rows([], _).
display_rows([Row|Remaining], RowIndex) :-
    write(' '), write(RowIndex), write('  '),
    display_row(Row),
    nl,
    NextRow is RowIndex + 1,
    display_rows(Remaining, NextRow).

% Display a single row
display_row([]).
display_row([Cell|Cells]) :-
    symbol(Cell, S),
    write(S), write(' '),
    display_row(Cells).

% -----------------------------------------------
% Display Current Player
% -----------------------------------------------

display_player(player1) :-
    write('Current Player: White'), nl.

display_player(player2) :-
    write('Current Player: Black'), nl.

% -----------------------------------------------
% Get Piece - Reading the Board
% -----------------------------------------------

% Get n-th element from a list (0-based index)
nthX([Head|_], 0, Head) :- !.
nthX([_|Tail], N, Value) :-
    N > 0,
    N1 is N - 1,
    nthX(Tail, N1, Value).

% Check if the piece is valid
get_piece(Board, X, Y, Piece) :-
    nthX(Board, Y, Row),
    nthX(Row, X, Piece).

% Check if the piece belongs to the current player
its_my_piece(Board, X-Y, Player) :-
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece).

% -----------------------------------------------
% Put Piece - Updating the Board
% -----------------------------------------------

% Base case
nth0(0, [Elem|Rest], Elem, Rest).
nth0(Index, [Head|Tail], Elem, [Head|NewTail]) :-
    Index > 0,
    Next is Index - 1,
    nth0(Next, Tail, Elem, NewTail).

put_piece(Board, X-Y, Piece, NewBoard) :-
    nth0(Y, Board, OldRow, RestRows),
    nth0(X, OldRow, _, OldRowRest),
    nth0(X, NewRow, Piece, OldRowRest),
    nth0(Y, NewBoard, NewRow, RestRows).

% -----------------------------------------------
% Valid Moves
% -----------------------------------------------

move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    repeat,
    choose_move(Board, SrcX-SrcY, DestX-DestY, Player),
    ( confirm(SrcX-SrcY, DestX-DestY) ->
        true
    ; write('Move cancelled. Starting over.'), nl, fail
    ).

% Prompt the user for a coordinate in the range 0..7
get_number(Min, Max, Prompt, X-Y) :-
    repeat,
    write(Prompt), nl,
    read(X-Y),
    X >= Min, X =< Max,
    Y >= Min, Y =< Max.

% Choose a piece
choose_move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    length(Board, Size),
    Max is Size - 1,
    repeat,
    get_number(0, Max, 'Source X-Y (e.g. 0-0)', SrcX-SrcY),
    ( its_my_piece(Board, SrcX-SrcY, Player) ->
        true
    ; write('Invalid piece. Please select your own piece.'), nl, fail
    ),
    get_number(0, Max, 'Destination X-Y (e.g. 0-0)', DestX-DestY),
    ( valid_move(Board, SrcX-SrcY, DestX-DestY, Player) ->
        true
    ; write('Invalid move. Please try again.'), nl, fail
    ),
    !.

% Confirm the move
confirm(SrcX-SrcY, DestX-DestY) :-
    write('Move from '), write(SrcX), write('-'), write(SrcY),
    write(' to '), write(DestX), write('-'), write(DestY), nl,
    write('Confirm? 1 - Yes; 0 - No'), nl,
    read(Choice),
    ( Choice = 1 -> true
    ; Choice = 0 -> fail
    ).

% -----------------------------------------------
% Game Loop and Game Over
% -----------------------------------------------
game_over(Board) :-
    \+ (member(Row, Board), member(w, Row)), % Sem peças brancas
    display_board(Board),
    write('Black wins!'), nl, !.
game_over(Board) :-
    \+ (member(Row, Board), member(b, Row)), % Sem peças pretas
    display_board(Board),
    write('White wins!'), nl, !.

game_loop((Board, CurrentPlayer)) :-
    game_over(Board), !.
game_loop((Board, CurrentPlayer)) :-
    display_board(Board),
    display_player(CurrentPlayer),
    move(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer),
    piece(CurrentPlayer, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    change_player(CurrentPlayer, NextPlayer),
    game_loop((NewBoard, NextPlayer)).

% -----------------------------------------------
% Menu and Game rules
% -----------------------------------------------

play :-
    board(2, Board),
    game_loop((Board, player1)).

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
menu_choice(_) :-
    write('Invalid choice!'), nl,
    menu.

rules :-
    write('Storm Clouds Rules:'), nl,
    write('- The game is played on an 8x8 board.'), nl,
    write('- Each player begins with 12 pieces.'), nl,
    write('- Non-capturing moves (1 tile):'), nl,
    write('  * Black moves up, diagonal-up-right, diagonal_up-left, right.'), nl,
    write('  * White moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('- Capturing moves (multiple tiles):'), nl,
    write('  * Black moves down, left, diagonal-down-left, diagonal-down-right.'), nl,
    write('  * White moves down, diagonal-down-left, left, diagonal-up-left.'), nl,
    write('- The objective is to eliminate all opponent pieces.'), nl,
    write('- If a player cannot move any piece, they must pass.'), nl.