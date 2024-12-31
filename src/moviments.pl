
% -----------------------------------------------
% Functions to check if the path is free
% -----------------------------------------------

within_board(X, Y) :- 
    X >= 0, X =< 7,
    Y >= 0, Y =< 7.


% free_path_black for Direction 1 (Down)
free_path_black(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    (
        NextY = DestY
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, X, NextY, empty),
        free_path_black(Board, X-NextY, X-DestY, 1)
    ).

% free_path_black for Direction 2 (Diagonal Down-Left)
free_path_black(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_black(Board, NextX-NextY, DestX-DestY, 2)
    ).

% free_path_black for Direction 3 (Left)
free_path_black(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    (
        NextX = DestX
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, Y, empty),
        free_path_black(Board, NextX-Y, DestX-Y, 3)
    ).

% free_path_black for Direction 4 (Diagonal Up-Left)
free_path_black(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_black(Board, NextX-NextY, DestX-DestY, 4)
    ).

% free_path_white for Direction 1 (Down)
free_path_white(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    (
        NextY = DestY
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, X, NextY, empty),
        free_path_white(Board, X-NextY, X-DestY, 1)
    ).

% free_path_white for Direction 2 (Diagonal Down-Left)
free_path_white(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_white(Board, NextX-NextY, DestX-DestY, 2)
    ).

% free_path_white for Direction 3 (Left)
free_path_white(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    (
        NextX = DestX
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, Y, empty),
        free_path_white(Board, NextX-Y, DestX-Y, 3)
    ).

% free_path_white for Direction 4 (Diagonal Up-Left)
free_path_white(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destination reached; do not require it to be empty
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_white(Board, NextX-NextY, DestX-DestY, 4)
    ).

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
    ),
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player1) :- 
    get_piece(Board, X, Y, w),
    (
      (Nx = X, Ny > Y), Direction = 1        % Down
    ; (Nx < X, Ny > Y), Direction = 2        % Diagonal down-left
    ; (Nx < X, Ny = Y), Direction = 3        % Left
    ; (Nx < X, Ny < Y), Direction = 4        % Diagonal up-left
    ),
    free_path_white(Board, X-Y, Nx-Ny, Direction),
    get_piece(Board, Nx, Ny, b), !.

% For Black player (player2)
valid_move(Board, X-Y, Nx-Ny, player2) :- 
    get_piece(Board, X, Y, b),
    (
        % Non-capturing moves (1 tile)
        (Nx is X, Ny is Y - 1)    % Up
    ;   (Nx is X + 1, Ny is Y - 1)  % Diagonal up-right
    ;   (Nx is X - 1, Ny is Y - 1)  % Diagonal up-left
    ;   (Nx is X + 1, Ny is Y)      % Right
    ),
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player2) :- 
    get_piece(Board, X, Y, b),
    (
      (Nx = X, Ny > Y) , Direction = 1      % Down
    ; (Nx < X, Ny > Y) , Direction = 2        % Left
    ; (Nx < X, Ny = Y) , Direction = 3       % Diagonal down-left
    ; (Nx > X, Ny > Y) , Direction = 4        % Diagonal down-right
    ),
    free_path_black(Board, X-Y, Nx-Ny, Direction),
    get_piece(Board, Nx, Ny, w), !.
