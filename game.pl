% -----------------------------------------------
% Imports
% -----------------------------------------------

:- use_module(library(lists)).
:- use_module(library(random)).


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
symbol(empty, '.').


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

random_list_move_member(List, Element) :-
    length(List, Length),
    Length > 0,
    random(0, Length, Index),
    nth0(Index, List, Element).

range(Min, Max, Min) :- Min =< Max.
range(Min, Max, Value) :-
    Min < Max,
    Next is Min + 1,
    range(Next, Max, Value).

% Escolhe uma peça aleatória e realiza um movimento aleatório entre os possíveis
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, _) :-
    valid_moves_list(Board, Player, MovePairs),
    MovePairs \= [],  % Assegura que há movimentos disponíveis
    % Extrai todas as posições de origem únicas
    findall(SrcX-SrcY, member(SrcX-SrcY-_-_, MovePairs), SrcListDup),
    sort(SrcListDup, SrcList),  % Remove duplicatas
    % Seleciona uma posição de origem aleatória
    random_member(SrcX-SrcY, SrcList),
    % Encontra todos os movimentos possíveis para a posição de origem selecionada
    findall(DestX-DestY, member(SrcX-SrcY-DestX-DestY, MovePairs), DestList),
    % Seleciona uma posição de destino aleatória
    random_member(DestX-DestY, DestList),
    % Realiza o movimento no tabuleiro
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    % Atualiza o tabuleiro após o movimento
    retractall(board(_, _)),
    assert(board(1, NewBoard)),  % Supondo que o tabuleiro atual seja referenciado por board(1, ...)
    % Exibe o movimento realizado pelo bot
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).


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
my_nth0(0, [Elem|Rest], Elem, Rest).
my_nth0(Index, [Head|Tail], Elem, [Head|NewTail]) :-
    Index > 0,
    Next is Index - 1,
    my_nth0(Next, Tail, Elem, NewTail).

put_piece(Board, X-Y, Piece, NewBoard) :-
    my_nth0(Y, Board, OldRow, RestRows),
    my_nth0(X, OldRow, _, OldRowRest),
    my_nth0(X, NewRow, Piece, OldRowRest),
    my_nth0(Y, NewBoard, NewRow, RestRows).

% -----------------------------------------------
% All Valid Moves List
% -----------------------------------------------

% Tenta mover para qualquer posição válida no tabuleiro
try_move_in_directions(Board, SrcX-SrcY, DestX-DestY, Player) :-
    % Itera sobre todas as possíveis coordenadas de destino
    range(0, 7, DestX),
    range(0, 7, DestY),
    % Evita que a posição de destino seja a mesma que a origem
    (DestX \= SrcX ; DestY \= SrcY),
    % Verifica se o movimento é válido de Src para Dest
    valid_move(Board, SrcX-SrcY, DestX-DestY, Player).


% Cria uma lista de pares com coordenadas de origem e destino de todos os movimentos válidos
valid_moves_list(Board, Player, MovePairs) :-
    findall(SrcX-SrcY-DestX-DestY,
        (
            range(0, 7, SrcX),
            range(0, 7, SrcY),
            get_piece(Board, SrcX, SrcY, Piece),
            piece(Player, Piece),
            try_move_in_directions(Board, SrcX-SrcY, DestX-DestY, Player)
        ),
        AllMoves
    ),
    sort(AllMoves, MovePairs).


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
    % Mostrar todas as jogadas possíveis
    write('Calculating all valid moves...'), nl,
    valid_moves_list(Board, Player, Moves),
    write('Valid Moves: '), nl,
    print_valid_moves(Moves),

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


print_valid_moves([]).
print_valid_moves([SrcX-SrcY-DestX-DestY]) :-
    format('From (~w, ~w) to (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).
print_valid_moves([SrcX-SrcY-DestX-DestY, SrcX2-SrcY2-DestX2-DestY2 | Rest]) :-
    format('From (~w, ~w) to (~w, ~w)    From (~w, ~w) to (~w, ~w)~n', [SrcX, SrcY, DestX, DestY, SrcX2, SrcY2, DestX2, DestY2]),
    print_valid_moves(Rest).
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
% Bot vs Bot Game Loop
% -----------------------------------------------
game_loop_bot_against_bot((Board, CurrentPlayer), Difficulty1, Difficulty2) :-
    game_over(Board), !.
game_loop_bot_against_bot((Board, CurrentPlayer), Difficulty1, Difficulty2) :-
    display_board(Board),
    display_player(CurrentPlayer),
        % Determine the bot's difficulty based on the current player
        (CurrentPlayer = player1 ->
            Difficulty = Difficulty1
        ;
            CurrentPlayer = player2 ->
            Difficulty = Difficulty2
        ),
        % Bot chooses a move
        ( choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, Difficulty) ->
            piece(CurrentPlayer, Piece),
            put_piece(Board, SrcX-SrcY, empty, TempBoard),
            put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
            change_player(CurrentPlayer, NextPlayer),
            game_loop_bot_against_bot((NewBoard, NextPlayer), Difficulty1, Difficulty2)
        ;
            % No valid move for current bot
            write('Bot '), write(CurrentPlayer), write(' has no valid moves!'), nl,
            change_player(CurrentPlayer, NextPlayer),
            game_loop_bot_against_bot((Board, NextPlayer), Difficulty1, Difficulty2)
    ).

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
play_player_vs_player :-
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

% Reset the board to the initial state
reset_board :-
    retractall(board(_, _)),
    initial_board(InitialBoard),
    assertz(board(1, InitialBoard)).

% Start the game
play :-
    reset_board,
    menu.

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

game_mode(1) :- play_player_vs_player.
game_mode(2) :- play_agaist_bot.
game_mode(3) :- play_bot_vs_bot.
game_mode(_) :- write('Invalid choice!'), nl, choose_game_mode.


% -----------------------------------------------
% Bots Difficulty
% -----------------------------------------------

