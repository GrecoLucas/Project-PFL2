% -----------------------------------------------
% Weak Bot
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
% Bots
% -----------------------------------------------


% -----------------------------------------------
% Players
% -----------------------------------------------

player(1, player1). % White
player(2, player2). % Black

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
    ),
    get_piece(Board, Nx, Ny, empty), !.

valid_move(Board, X-Y, Nx-Ny, player1) :- 
    get_piece(Board, X, Y, w),
    (
      (Nx = X, Ny > Y)        % Down
    ; (Nx < X, Ny > Y)        % Diagonal down-left
    ; (Nx < X, Ny = Y)        % Left
    ; (Nx < X, Ny < Y)        % Diagonal up-left
    ),
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
      (Nx = X, Ny > Y)         % Down
    ; (Nx < X, Ny = Y)         % Left
    ; (Nx < X, Ny > Y)         % Diagonal down-left
    ; (Nx > X, Ny > Y)         % Diagonal down-right
    ),
    get_piece(Board, Nx, Ny, w), !.


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
% Weak Bot Moves
% -----------------------------------------------
% Encontra todos os movimentos válidos para o jogador atual
:- use_module(library(random)).

range(Min, Max, Min) :- Min =< Max.
range(Min, Max, Value) :-
    Min < Max,
    Next is Min + 1,
    range(Next, Max, Value).

choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, _) :-
    find_piece_and_move(Board, CurrentPlayer, SrcX-SrcY, DestX-DestY).

% Encontra a primeira peça do jogador que pode ser movida e realiza o movimento
find_piece_and_move(Board, Player, SrcX-SrcY, DestX-DestY) :-
    range(0, 7, SrcX),
    range(0, 7, SrcY),
    get_piece(Board, SrcX, SrcY, Piece),
    piece(Player, Piece), % Verifica se a peça pertence ao jogador
    try_move_in_directions(Board, SrcX-SrcY, DestX-DestY, Player).

% Tenta mover para qualquer uma das 8 direções
try_move_in_directions(Board, SrcX-SrcY, DestX-DestY, Player) :-
    member(Direction, [
        -1-0, 1-0, 0-1, 0-(-1), % Verticais e horizontais
        -1-1, -1-(-1), 1-1, 1-(-1) % Diagonais
    ]),
    Direction = DX-DY,
    DestX is SrcX + DX,
    DestY is SrcY + DY,
    valid_move(Board, SrcX-SrcY, DestX-DestY, Player), % Verifica se o movimento é válido
    !.

% Caso nenhuma direção seja válida para uma peça, falha e passa para a próxima
try_move_in_directions(_, _, _, _) :- fail.

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

% Check if a piece has any available moves
has_available_moves(Board, X-Y, player1) :-
    member(Direction, [
        0-(-1), 1-(-1), 1-0, 1-1, % Non-capturing moves for White
        0-1, -1-1, -1-0, -1-(-1)  % Capturing moves for White
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    (valid_move(Board, X-Y, DestX-DestY, player1) -> true ; fail).

has_available_moves(Board, X-Y, player2) :-
    member(Direction, [
        0-(-1), 1-(-1), -1-(-1), 1-0, % Non-capturing moves for Black
        0-1, -1-0, -1-1, 1-1           % Capturing moves for Black
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    (valid_move(Board, X-Y, DestX-DestY, player2) -> true ; fail).

% Check if the piece belongs to the current player
valid_piece(Board, X-Y, Player) :-
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece),
    has_available_moves(Board, X-Y, Player).

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
    ( valid_piece(Board, SrcX-SrcY, Player) ->
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
% Game Over
% -----------------------------------------------
game_over(Board) :-
    \+ (member(Row, Board), member(w, Row)), % Sem peças brancas
    display_board(Board),
    write('Black wins!'), nl, !.
game_over(Board) :-
    \+ (member(Row, Board), member(b, Row)), % Sem peças pretas
    display_board(Board),
    write('White wins!'), nl, !.


% -----------------------------------------------
% PvP Game loop
% -----------------------------------------------
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
% PvBot Game loop
% -----------------------------------------------

game_loop_against_bot((Board, player2), _) :-
    game_over(Board), !.
game_loop_against_bot((Board, CurrentPlayer), Difficulty) :-
    display_board(Board),
    display_player(CurrentPlayer),
    (
        % Verifica se o jogador atual é o bot
        CurrentPlayer = player2 ->
        (
            % Bot escolhe um movimento
            choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, Difficulty) ->
                piece(CurrentPlayer, Piece),
                put_piece(Board, SrcX-SrcY, empty, TempBoard),
                put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
                change_player(CurrentPlayer, NextPlayer),
                game_loop_against_bot((NewBoard, NextPlayer), Difficulty)
            ;
                write('Bot has no valid moves!'), nl,
                change_player(CurrentPlayer, NextPlayer),
                game_loop_against_bot((Board, NextPlayer), Difficulty)
        )
        ;
        % Caso contrário, é a vez do jogador humano
        (
            move(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer) ->
                piece(CurrentPlayer, Piece),
                put_piece(Board, SrcX-SrcY, empty, TempBoard),
                put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
                change_player(CurrentPlayer, NextPlayer),
                game_loop_against_bot((NewBoard, NextPlayer), Difficulty)
            ;
                write('Invalid move! Try again.'), nl,
                game_loop_against_bot((Board, CurrentPlayer), Difficulty)
        )
    ).

    

% -----------------------------------------------
% BotvBot Game loop
% -----------------------------------------------

% nao funciona ainda
%game_loop_bot_against_bot((Board, player1), Difficulty1, Difficulty2) :-
%    game_over(Board), !.
%game_loop_bot_against_bot((Board, player1), Difficulty1, Difficulty2) :-
%    display_board(Board),
%    display_player(player1),
%    choose_move(Board, SrcX-SrcY, DestX-DestY, player1),
%    piece(player1, Piece),
%    put_piece(Board, SrcX-SrcY, empty, TempBoard),
%    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
%    game_loop_bot_against_bot((NewBoard, player2), Difficulty1, Difficulty2).


% -----------------------------------------------
% Choose Game Mode
% -----------------------------------------------
choose_game_mode :-
    write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Bot'), nl,
    write('3. Bot vs Bot'), nl,
    read(GameMode),
    game_mode(GameMode).


% -----------------------------------------------
% Plays
% -----------------------------------------------
play :-
    board(1, Board),
    game_loop((Board, player1)).


play_agaist_bot :-
    write('Choose difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty),
    write('Bot difficulty: '), write(Difficulty), nl,
    board(1, Board),
    game_loop_against_bot((Board, player1), Difficulty).

play_bot_vs_bot :-
    write('Bot vs Bot'), nl,
    write('Choose difficulty for bot 1:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty1),
    write('Choose difficulty for bot 2:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl,
    read(Difficulty2),
    board(1, Board),
    game_loop_bot_against_bot((Board, player1), Difficulty1, Difficulty2).


% -----------------------------------------------
% Rules and Menu
% -----------------------------------------------

menu :-
    write('Welcome to Storm Clouds!'), nl,
    write('1. Play'), nl,
    write('2. Rules'), nl,
    write('3. Exit'), nl,
    read(Choice),
    menu_choice(Choice).

rules :-
    write('Storm Clouds Rules:'), nl,
    write('- The game is played on an 8x8 board.'), nl,
    write('- Each player begins with 12 pieces.'), nl,
    write('- Non-capturing moves (only move 1 tile):'), nl,
    write('  * Black moves up, diagonal-up-right, diagonal_up-left, right.'), nl,
    write('  * White moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('- Capturing moves (can move thogh multiple tiles):'), nl,
    write('  * Black moves down, left, diagonal-down-left, diagonal-down-right.'), nl,
    write('  * White moves down, diagonal-down-left, left, diagonal-up-left.'), nl,
    write('- The objective is to eliminate all opponent pieces.'), nl,
    write('- If a player cannot move any piece, they must pass.'), nl.


% -----------------------------------------------
% Chooses
% -----------------------------------------------

menu_choice(1) :- choose_game_mode.
menu_choice(2) :- rules, menu.
menu_choice(3) :- write('Goodbye!'), nl.
menu_choice(_) :-
    write('Invalid choice!'), nl,
    menu.

game_mode(1) :- play.
game_mode(2) :- play_agaist_bot.
game_mode(3) :- play_bot_vs_bot.
game_mode(_) :- write('Invalid choice!'), nl, choose_game_mode.


% -----------------------------------------------
% Bots Difficulty
% -----------------------------------------------

