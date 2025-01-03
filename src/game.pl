% -----------------------------------------------
% Imports
% -----------------------------------------------

:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('moviments.pl').
:- consult('data.pl').
:- consult('bots.pl').

% -----------------------------------------------
% Display Board
% -----------------------------------------------

% Display the entire board with column/row labels
display_board(Board) :-
    nl,
    write('   +---+---+---+---+---+---+---+---+'), nl,
    display_rows(Board, 0),
    write('     1   2   3   4   5   6   7   8'), nl, nl.

% Recursively print each row with index
display_rows([], _).
display_rows([Row|Remaining], RowIndex) :-
    N is 8-RowIndex,
    write(' '), write(N), write(' |'),
    display_row(Row),nl,
    write('   +---+---+---+---+---+---+---+---+'),nl,
    NextRow is RowIndex + 1,
    display_rows(Remaining, NextRow).

% Display a single row
display_row([]).
display_row([Cell|Cells]) :-
    symbol(Cell, S),
    write(' '), write(S), write(' |'),
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

% Check if a piece has any available moves
has_available_moves(Board, X-Y, player1) :-
    member(Direction, [
        0-(-1), 1-(-1), 1-0, 1-1, % Non-capturing moves for White
        0-1, -1-1, -1-0, -1-(-1)  % Capturing moves for White
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    valid_move(Board, X-Y, DestX-DestY, player1).

has_available_moves(Board, X-Y, player2) :-
    member(Direction, [
        0-(-1), 1-(-1), -1-(-1), 1-0, % Non-capturing moves for Black
        0-1, -1-0, -1-1, 1-1           % Capturing moves for Black
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    valid_move(Board, X-Y, DestX-DestY, player2).

% Check if the piece belongs to the current player
valid_piece(Board, X-Y, Player) :-
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece),
    has_available_moves(Board, X-Y, Player).

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
    ; nl, write('Move cancelled. Starting over.'), nl, fail
    ).

% Prompt the user for a move number
get_move_number(Moves, Move) :-
    length(Moves, Length),
    MaxIndex is Length - 1,
    repeat,
    nl,write('Choose a move number (0-'), write(MaxIndex), write('): '), nl,
    read(Index),
    Index >= 0, Index =< MaxIndex,
    nth0(Index, Moves, Move).

% Choose a piece and move
choose_move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    % Mostrar todas as jogadas possíveis
    nl,write('Calculating all valid moves...'), nl, nl,
    valid_moves_list(Board, Player, Moves),
    write('Valid Moves: '), nl,
    print_valid_moves(Moves, 0),

    % Get the move number from the user
    get_move_number(Moves, SrcX-SrcY-DestX-DestY),
    !.

% Confirm the move
confirm(SrcX-SrcY, DestX-DestY) :-
    SrcX1 is SrcX + 1,
    SrcY1 is 8-SrcY,
    DestX1 is DestX + 1,
    DestY1 is 8-DestY,
    nl,write('Move from '), write(SrcX1), write('-'), write(SrcY1),
    write(' to '), write(DestX1), write('-'), write(DestY1), nl, nl,
    write('Confirm? 1 - Yes; 0 - No'), nl,
    read(Choice),
    ( Choice = 1 -> true
    ; Choice = 0 -> fail
    ).

print_valid_moves(Moves) :-
    print_valid_moves(Moves, 0).

print_valid_moves([], _).
print_valid_moves([SrcX-SrcY-DestX-DestY | Rest], Index) :-
    SrcX1 is SrcX + 1,
    SrcY1 is 8-SrcY,
    DestX1 is DestX + 1,
    DestY1 is 8-DestY,
    format('~w: From (~w, ~w) to (~w, ~w)~n', [Index, SrcX1, SrcY1, DestX1, DestY1]),
    NextIndex is Index + 1,
    print_valid_moves(Rest, NextIndex).


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
            choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, Difficulty, NewBoard) -> 
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
    ( choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, Difficulty, NewBoard) -> 
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
    nl, write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Bot'), nl,
    write('3. Bot vs Bot'), nl,
    read(GameMode),
    game_mode(GameMode).

% -----------------------------------------------
% Plays
% -----------------------------------------------

play_player_vs_player :-
    initial_board(InitialBoard),
    game_loop((InitialBoard, player1)).

play_agaist_bot :-
    write('Choose difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl, nl,
    read(Difficulty),
    write('Bot difficulty: '), write(Difficulty), nl, nl,
    initial_board(InitialBoard),
    game_loop_against_bot((InitialBoard, player1), Difficulty).

play_bot_vs_bot :-
    write('Bot vs Bot'), nl,
    write('Choose difficulty for bot 1:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl, nl,
    read(Difficulty1),
    write('Choose difficulty for bot 2:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl, nl,
    read(Difficulty2),
    initial_board(InitialBoard),
    game_loop_bot_against_bot((InitialBoard, player1), Difficulty1, Difficulty2).

% -----------------------------------------------
% Rules and Menu
% -----------------------------------------------

% Reset the board to the initial state
reset_board(InitialBoard) :-
    initial_board(InitialBoard).

% Start the game
play :-
    reset_board(InitialBoard),
    menu.

menu :-
    nl,
    write('--------------------------'), nl,
    write(' Welcome to Storm Clouds!'), nl,
    write('--------------------------'), nl,
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
    write('  * Black moves up, diagonal-up-right, diagonal-up-left, right.'), nl,
    write('  * White moves up, diagonal-up-right, right, diagonal-down-right.'), nl,
    write('- Capturing moves (can move through multiple tiles):'), nl,
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

:- initialization(play).