% -----------------------------------------------
% 3. Display Board
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
% 4. Display Current Player
% -----------------------------------------------

display_player(player1) :-
    write('Current Player: White'), nl.

display_player(player2) :-
    write('Current Player: Black'), nl.

% -----------------------------------------------
% 5. Utility Predicates
% -----------------------------------------------

% Get n-th element from a list (0-based index)
nthX([Head|_], 0, Head) :- !.
nthX([_|Tail], N, Value) :-
    N > 0,
    N1 is N - 1,
    nthX(Tail, N1, Value).

get_piece(Board, X, Y, Piece) :-
    nthX(Board, Y, Row),
    nthX(Row, X, Piece).

% Check if the piece belongs to the current player
its_my_piece(Board, X-Y, Player) :-
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece).

% Valid moves for White (player1)
valid_move(Board, X-Y, Nx-Ny, player1) :-
    get_piece(Board, X, Y, w),
    (
      % Non-capturing
      (Nx is X,     Ny is Y - 1)    % up
    ; (Nx is X - 1, Ny is Y - 1)    % diagonal-up-left
    ; (Nx is X + 1, Ny is Y)        % right
    ; (Nx is X + 1, Ny is Y - 1)    % diagonal-up-right

      % Capturing
    ; (Nx > X, Ny is Y + (Nx - X))  % down
    ; (Nx < X, Ny is Y + (X - Nx))  % diagonal-down-left
    ; (Nx < X, Ny is Y)             % left
    ; (Nx < X, Ny is Y + (X - Nx))  % diagonal-down-left
    ),
    get_piece(Board, Nx, Ny, empty) ; get_piece(Board, Nx, Ny, b).

% For Black (player2)
valid_move(Board, X-Y, Nx-Ny, player2) :-
    get_piece(Board, X, Y, b),
    (
      % Non-capturing
      (Nx is X,     Ny is Y - 1)    % up
    ; (Nx is X + 1, Ny is Y - 1)    % diagonal-up-right
    ; (Nx is X + 1, Ny is Y)        % right
    ; (Nx is X + 1, Ny is Y + 1)    % diagonal-down-right

      % Capturing
    ; (Nx > X, Ny is Y + (Nx - X))  % down
    ; (Nx < X, Ny is Y + (X - Nx))  % diagonal-up-left
    ; (Nx < X, Ny is Y)             % left
    ; (Nx < X, Ny is Y + (X - Nx))  % diagonal-down-left
    ),
    get_piece(Board, Nx, Ny, empty) ; get_piece(Board, Nx, Ny, w).

its_my_piece(Board, X-Y, Piece, Player) :-
    write('Checking if '), write(Piece), write(' belongs to '), write(Player), nl,
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece).


% -----------------------------------------------
% 6. Put Piece - Updating the Board
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
% 7. Game Loop
% -----------------------------------------------

% Prompt the user for a coordinate in the range 0..7
get_number(Min, Max, Prompt, N) :-
    repeat,
    write(Prompt), write(': '),
    read(N),
    integer(N),
    N >= Min,
    N =< Max,
    !.

% Choose a move
choose_move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    length(Board, Size),
    Max is Size - 1,
    repeat,
    get_number(0, Max, 'Source X', SrcX),
    get_number(0, Max, 'Source Y', SrcY),
    ( its_my_piece(Board, SrcX-SrcY, Player) ->
        true
    ; write('Invalid piece. Please select your own piece.'), nl, fail
    ),
    get_number(0, Max, 'Destination X', DestX),
    get_number(0, Max, 'Destination Y', DestY),
    ( valid_move(Board, SrcX-SrcY, DestX-DestY, Player) ->
        true
    ; write('Invalid move. Please try again.'), nl, fail
    ),
    !.

game_loop((Board, CurrentPlayer)) :-
    display_board(Board),
    display_player(CurrentPlayer),
    choose_move(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer),
    piece(CurrentPlayer, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    change_player(CurrentPlayer, NextPlayer),
    game_loop((NewBoard, NextPlayer)).

% -----------------------------------------------
% 8. Menu
% -----------------------------------------------

play :-
    board(1, Board),
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
    write('  * Black moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('  * White moves up, diagonal-up-left, right, diagonal-up-right.'), nl,
    write('- Capturing moves (multiple tiles):'), nl,
    write('  * Black moves down, diagonal-up-left, left, diagonal-down-left.'), nl,
    write('  * White moves down, diagonal-down-right, left, diagonal-down-left.'), nl,
    write('- The objective is to eliminate all opponent pieces.'), nl,
    write('- If a player cannot move any piece, they must pass.'), nl.