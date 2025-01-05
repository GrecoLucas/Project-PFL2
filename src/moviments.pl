
% -----------------------------------------------
% Functions to check if the path is free
% -----------------------------------------------

within_board(X, Y) :- 
    X >= 0, X =< 7,
    Y >= 0, Y =< 7.


% Black Down Movement
free_path_black_dest_reached(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    NextY = DestY.

free_path_black_continue(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    get_piece(Board, X, NextY, empty),
    free_path_black(Board, X-NextY, X-DestY, 1).

free_path_black(Board, X-Y, X-DestY, 1) :-
    free_path_black_dest_reached(Board, X-Y, X-DestY, 1).
free_path_black(Board, X-Y, X-DestY, 1) :-
    free_path_black_continue(Board, X-Y, X-DestY, 1).

% Black Diagonal Down-Left
free_path_black_dest_reached(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    NextX = DestX,
    NextY = DestY.

free_path_black_continue(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    get_piece(Board, NextX, NextY, empty),
    free_path_black(Board, NextX-NextY, DestX-DestY, 2).

free_path_black(Board, X-Y, DestX-DestY, 2) :-
    free_path_black_dest_reached(Board, X-Y, DestX-DestY, 2).
free_path_black(Board, X-Y, DestX-DestY, 2) :-
    free_path_black_continue(Board, X-Y, DestX-DestY, 2).

% Black Left
free_path_black_dest_reached(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    NextX = DestX.

free_path_black_continue(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    get_piece(Board, NextX, Y, empty),
    free_path_black(Board, NextX-Y, DestX-Y, 3).

free_path_black(Board, X-Y, DestX-Y, 3) :-
    free_path_black_dest_reached(Board, X-Y, DestX-Y, 3).
free_path_black(Board, X-Y, DestX-Y, 3) :-
    free_path_black_continue(Board, X-Y, DestX-Y, 3).

% Black Diagonal Up-Left  
free_path_black_dest_reached(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    NextX = DestX,
    NextY = DestY.

free_path_black_continue(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    get_piece(Board, NextX, NextY, empty),
    free_path_black(Board, NextX-NextY, DestX-DestY, 4).

free_path_black(Board, X-Y, DestX-DestY, 4) :-
    free_path_black_dest_reached(Board, X-Y, DestX-DestY, 4).
free_path_black(Board, X-Y, DestX-DestY, 4) :-
    free_path_black_continue(Board, X-Y, DestX-DestY, 4).

% White movements follow same pattern
% White Down
free_path_white_dest_reached(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    NextY = DestY.

free_path_white_continue(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    get_piece(Board, X, NextY, empty),
    free_path_white(Board, X-NextY, X-DestY, 1).

free_path_white(Board, X-Y, X-DestY, 1) :-
    free_path_white_dest_reached(Board, X-Y, X-DestY, 1).
free_path_white(Board, X-Y, X-DestY, 1) :-
    free_path_white_continue(Board, X-Y, X-DestY, 1).

% White Diagonal Down-Left
free_path_white_dest_reached(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    NextX = DestX,
    NextY = DestY.

free_path_white_continue(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    get_piece(Board, NextX, NextY, empty),
    free_path_white(Board, NextX-NextY, DestX-DestY, 2).

free_path_white(Board, X-Y, DestX-DestY, 2) :-
    free_path_white_dest_reached(Board, X-Y, DestX-DestY, 2).
free_path_white(Board, X-Y, DestX-DestY, 2) :-
    free_path_white_continue(Board, X-Y, DestX-DestY, 2).

% White Left
free_path_white_dest_reached(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    NextX = DestX.

free_path_white_continue(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    get_piece(Board, NextX, Y, empty),
    free_path_white(Board, NextX-Y, DestX-Y, 3).

free_path_white(Board, X-Y, DestX-Y, 3) :-
    free_path_white_dest_reached(Board, X-Y, DestX-Y, 3).
free_path_white(Board, X-Y, DestX-Y, 3) :-
    free_path_white_continue(Board, X-Y, DestX-Y, 3).

% White Diagonal Up-Left
free_path_white_dest_reached(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    NextX = DestX,
    NextY = DestY.

free_path_white_continue(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    get_piece(Board, NextX, NextY, empty),
    free_path_white(Board, NextX-NextY, DestX-DestY, 4).

free_path_white(Board, X-Y, DestX-DestY, 4) :-
    free_path_white_dest_reached(Board, X-Y, DestX-DestY, 4).
free_path_white(Board, X-Y, DestX-DestY, 4) :-
    free_path_white_continue(Board, X-Y, DestX-DestY, 4).
% -----------------------------------------------
% Movement Rules
% -----------------------------------------------

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Up
    get_piece(Board, X, Y, w),
    Nx is X,
    Ny is Y - 1,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Diagonal up-right
    get_piece(Board, X, Y, w),
    Nx is X + 1,
    Ny is Y - 1,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Right
    get_piece(Board, X, Y, w),
    Nx is X + 1,
    Ny is Y,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Diagonal down-right
    get_piece(Board, X, Y, w),
    Nx is X + 1,
    Ny is Y + 1,
    get_piece(Board, Nx, Ny, empty), !.

% White capturing moves
valid_move(Board, X-Y, Nx-Ny, player1) :-  % Down
    get_piece(Board, X, Y, w),
    Nx = X,
    Ny > Y,
    free_path_white(Board, X-Y, Nx-Ny, 1),
    get_piece(Board, Nx, Ny, b), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Diagonal down-left
    get_piece(Board, X, Y, w),
    Nx < X,
    Ny > Y,
    free_path_white(Board, X-Y, Nx-Ny, 2),
    get_piece(Board, Nx, Ny, b), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Left
    get_piece(Board, X, Y, w),
    Nx < X,
    Ny = Y,
    free_path_white(Board, X-Y, Nx-Ny, 3),
    get_piece(Board, Nx, Ny, b), !.

valid_move(Board, X-Y, Nx-Ny, player1) :-  % Diagonal up-left
    get_piece(Board, X, Y, w),
    Nx < X,
    Ny < Y,
    free_path_white(Board, X-Y, Nx-Ny, 4),
    get_piece(Board, Nx, Ny, b), !.

% Black non-capturing moves
valid_move(Board, X-Y, Nx-Ny, player2) :-  % Up
    get_piece(Board, X, Y, b),
    Nx is X,
    Ny is Y - 1,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Diagonal up-right
    get_piece(Board, X, Y, b),
    Nx is X + 1,
    Ny is Y - 1,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Diagonal up-left
    get_piece(Board, X, Y, b),
    Nx is X - 1,
    Ny is Y - 1,
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Right
    get_piece(Board, X, Y, b),
    Nx is X + 1,
    Ny is Y,
    get_piece(Board, Nx, Ny, empty), !.

% Black capturing moves
valid_move(Board, X-Y, Nx-Ny, player2) :-  % Down
    get_piece(Board, X, Y, b),
    Nx = X,
    Ny > Y,
    free_path_black(Board, X-Y, Nx-Ny, 1),
    get_piece(Board, Nx, Ny, w), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Left
    get_piece(Board, X, Y, b),
    Nx < X,
    Ny > Y,
    free_path_black(Board, X-Y, Nx-Ny, 2),
    get_piece(Board, Nx, Ny, w), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Diagonal down-left
    get_piece(Board, X, Y, b),
    Nx < X,
    Ny = Y,
    free_path_black(Board, X-Y, Nx-Ny, 3),
    get_piece(Board, Nx, Ny, w), !.

valid_move(Board, X-Y, Nx-Ny, player2) :-  % Diagonal down-right
    get_piece(Board, X, Y, b),
    Nx > X,
    Ny > Y,
    free_path_black(Board, X-Y, Nx-Ny, 4),
    get_piece(Board, Nx, Ny, w), !.