% -----------------------------------------------
% Imports
% -----------------------------------------------

:- use_module(library(lists)).
:- use_module(library(random)).
:- consult('moviments.pl').
:- consult('data.pl').
:- consult('bots.pl').

% -----------------------------------------------
% Mostrar o tabuleiro
% -----------------------------------------------

% Mostrar o tabuleiro
% Board = Tabuleiro a ser exibido
display_board(Board) :-
    nl,
    write('     0   1   2   3   4   5   6   7'), nl,
    write('   +---+---+---+---+---+---+---+---+'), nl,
    display_rows(Board, 0), nl.

% Mostrar as linhas do tabuleiro
% Rows = Linhas a serem exibidas
% RowIndex = Índice da linha atual
display_rows([], _).
display_rows([Row|Remaining], RowIndex) :-
    write(' '), write(RowIndex), write(' |'),
    display_row(Row),nl,
    write('   +---+---+---+---+---+---+---+---+'),nl,
    NextRow is RowIndex + 1,
    display_rows(Remaining, NextRow).

% Mostrar as células de uma linha
% Cells = Células a serem exibidas
display_row([]).
display_row([Cell|Cells]) :-
    symbol(Cell, S),
    write(' '), write(S), write(' |'),
    display_row(Cells).

% -----------------------------------------------
% Mostrar o jogador atual
% -----------------------------------------------

display_player(player1) :-
    write('Current Player: White'), nl.

display_player(player2) :-
    write('Current Player: Black'), nl.

% -----------------------------------------------
% ler peça
% -----------------------------------------------

% ler peça
% N = Índice da peça

nthX([Head|_], 0, Head) :- !.
nthX([_|Tail], N, Value) :-
    N > 0,
    N1 is N - 1,
    nthX(Tail, N1, Value).

% Verifica se a peça é válida
% Board = Tabuleiro atual
% X-Y = Coordenadas da peça
% Piece = Peça a ser verificada
get_piece(Board, X, Y, Piece) :-
    nthX(Board, Y, Row),
    nthX(Row, X, Piece).

% Verifica se uma peça tem movimentos disponíveis
% Board = Tabuleiro atual
% X-Y = Coordenadas da peça
% Player = Jogador atual
has_available_moves(Board, X-Y, player1) :-
    member(Direction, [
        0-(-1), 1-(-1), 1-0, 1-1, % Movimentos não capturantes para Branco
        0-1, -1-1, -1-0, -1-(-1)  % Movimentos capturantes para Branco
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    valid_move(Board, X-Y, DestX-DestY, player1).

% Verifica se uma peça tem movimentos disponíveis
% Board = Tabuleiro atual
% X-Y = Coordenadas da peça
% Player = Jogador atual
has_available_moves(Board, X-Y, player2) :-
    member(Direction, [
        0-(-1), 1-(-1), -1-(-1), 1-0, % Movimentos não capturantes para Preto
        0-1, -1-0, -1-1, 1-1           % Movimentos capturantes para Preto
    ]),
    Direction = DX-DY,
    DestX is X + DX,
    DestY is Y + DY,
    valid_move(Board, X-Y, DestX-DestY, player2).

% Verifica se a peça pertence ao jogador atual
% Player = Jogador atual
% Piece = Peça a ser verificada
valid_piece(Board, X-Y, Player) :-
    get_piece(Board, X, Y, Piece),
    piece(Player, Piece),
    has_available_moves(Board, X-Y, Player).

% -----------------------------------------------
% Alterar peça 
% -----------------------------------------------

% Altera a peça em uma posição específica
% Index = Índice da peça
% [Elem|Rest] = Lista de peças
% Elem = Peça a ser alterada
% Rest = Lista de peças restantes
my_nth0(0, [Elem|Rest], Elem, Rest).
my_nth0(Index, [Head|Tail], Elem, [Head|NewTail]) :-
    Index > 0,
    Next is Index - 1,
    my_nth0(Next, Tail, Elem, NewTail).

% Colocar peça em uma posição específica
% Board = Tabuleiro atual
% X-Y = Coordenadas da peça
% Piece = Peça a ser colocada
% NewBoard = Novo tabuleiro após a peça ser colocada
put_piece(Board, X-Y, Piece, NewBoard) :-
    my_nth0(Y, Board, OldRow, RestRows),
    my_nth0(X, OldRow, _, OldRowRest),
    my_nth0(X, NewRow, Piece, OldRowRest),
    my_nth0(Y, NewBoard, NewRow, RestRows).

% -----------------------------------------------
% All Valid Moves List
% -----------------------------------------------

% Intera sobre o intervalo de valores do tamanho do tabuleiro (0 a 7)
% Min = Valor mínimo do intervalo
% Max = Valor máximo do intervalo
% Value = Valor gerado dentro do intervalo
range(Min, Max, Min) :- Min =< Max. 
range(Min, Max, Value) :-
    Min < Max,  
    Next is Min + 1, 
    range(Next, Max, Value).  

% Tenta mover para qualquer posição válida no tabuleiro, combinação 0-7 para X e Y
% Board = Estado atual do tabuleiro
% SrcX-SrcY = Coordenadas de origem da peça
% DestX-DestY = Coordenadas de destino da peça
% Player = Jogador atual
try_move_in_directions(Board, SrcX-SrcY, DestX-DestY, Player) :-
    range(0, 7, DestX),
    range(0, 7, DestY),
    (DestX \= SrcX ; DestY \= SrcY),
    valid_move(Board, SrcX-SrcY, DestX-DestY, Player).

% Cria uma lista de pares com coordenadas de origem e destino de todos os movimentos válidos
% Board = Estado atual do tabuleiro
% Player = Jogador atual
% MovePairs = Lista de movimentos válidos para o jogador atual
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
% Movimento válido
% -----------------------------------------------

% Verifica se um movimento é válido
% Board = Tabuleiro atual
% SrcX-SrcY = Coordenadas de origem
% DestX-DestY = Coordenadas de destino
% Player = Jogador atual
move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    repeat,
    choose_move(Board, SrcX-SrcY, DestX-DestY, Player),
    ( confirm(SrcX-SrcY, DestX-DestY) ->
        true
    ; nl, write('Move cancelled. Starting over.'), nl, fail
    ).

% Obter o número do movimento do usuário
% Moves = Lista de movimentos válidos
% Move = Movimento escolhido pelo usuário
get_move_number(Moves, Move) :-
    length(Moves, Length),
    MaxIndex is Length - 1,
    repeat,
    nl,write('Choose a move number (0-'), write(MaxIndex), write('): '), nl,
    read(Index),
    Index >= 0, Index =< MaxIndex,
    nth0(Index, Moves, Move).

% Escolher um movimento
% Board = Tabuleiro atual
% SrcX-SrcY = Coordenadas de origem
% DestX-DestY = Coordenadas de destino
% Player = Jogador atual
choose_move(Board, SrcX-SrcY, DestX-DestY, Player) :-
    nl,write('Calculating all valid moves...'), nl, nl,
    valid_moves_list(Board, Player, Moves),
    write('Valid Moves: '), nl,
    print_valid_moves(Moves, 0),
    get_move_number(Moves, SrcX-SrcY-DestX-DestY),
    !.

% Confirmar o movimento
% SrcX-SrcY = Coordenadas de origem
% DestX-DestY = Coordenadas de destino
confirm(SrcX-SrcY, DestX-DestY) :-
    nl,write('Move from '), write(SrcX), write('-'), write(SrcY),
    write(' to '), write(DestX), write('-'), write(DestY), nl, nl,
    write('Confirm? 1 - Yes; 0 - No'), nl,
    read(Choice),
    ( Choice = 1 -> true
    ; Choice = 0 -> fail
    ).

print_valid_moves(Moves) :-
    print_valid_moves(Moves, 0).

% Imprime os movimentos válidos
% Moves = Lista de movimentos válidos
% Index = Índice do movimento atual
print_valid_moves([], _).
print_valid_moves([SrcX-SrcY-DestX-DestY | Rest], Index) :-
    format('~w: From (~w, ~w) to (~w, ~w)~n', [Index, SrcX, SrcY, DestX, DestY]),
    NextIndex is Index + 1,
    print_valid_moves(Rest, NextIndex).


% -----------------------------------------------
% Game Over
% -----------------------------------------------

% Verifica se o jogo acabou
% Board = Tabuleiro atual
game_over(Board) :-
    \+ (member(Row, Board), member(w, Row)), % Sem peças brancas
    display_board(Board),
    write('Black wins!'), nl, !.
game_over(Board) :-
    \+ (member(Row, Board), member(b, Row)), % Sem peças pretas
    display_board(Board),
    write('White wins!'), nl, !.


% -----------------------------------------------
% Funções principais
% -----------------------------------------------
% PvP Game loop
% -----------------------------------------------

% Loop do jogo Player vs Player
% Board = Tabuleiro atual
% CurrentPlayer = Jogador atual
game_loop((Board, CurrentPlayer)) :-
    game_over(Board), !.
game_loop((Board, CurrentPlayer)) :-
    display_board(Board),                                   % Mostra o tabuleiro
    display_player(CurrentPlayer),                          % Mostra o jogador atual              
    move(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer),     % Realiza um movimento
    piece(CurrentPlayer, Piece),                            % Obtém a peça do jogador atual                 
    put_piece(Board, SrcX-SrcY, empty, TempBoard),          % Remove a peça da posição de origem
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),     % Coloca a peça na posição de destino
    change_player(CurrentPlayer, NextPlayer),               % Troca o jogador
    game_loop((NewBoard, NextPlayer)).                      % Continua o loop do jogo


% -----------------------------------------------
% PvBot Game loop
% -----------------------------------------------

% Loop do jogo Player vs Bot
% Board = Tabuleiro atual
% CurrentPlayer = Jogador atual
% Difficulty = Dificuldade do bot
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

% Loop do jogo Bot vs Bot
% Board = Tabuleiro atual
% CurrentPlayer = Jogador atual
% Difficulty1 = Dificuldade do bot 1
% Difficulty2 = Dificuldade do bot 2
game_loop_bot_against_bot((Board, CurrentPlayer), Difficulty1, Difficulty2) :-
    game_over(Board), !.
game_loop_bot_against_bot((Board, CurrentPlayer), Difficulty1, Difficulty2) :-
    display_board(Board),
    display_player(CurrentPlayer),
    % Determina a dificuldade do bot com base no jogador atual
    (CurrentPlayer = player1 -> 
        Difficulty = Difficulty1
    ; 
        CurrentPlayer = player2 -> 
        Difficulty = Difficulty2
    ),
    ( choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, CurrentPlayer, Difficulty, NewBoard) -> 
        piece(CurrentPlayer, Piece),
        put_piece(Board, SrcX-SrcY, empty, TempBoard),
        put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
        change_player(CurrentPlayer, NextPlayer),
        game_loop_bot_against_bot((NewBoard, NextPlayer), Difficulty1, Difficulty2)
    ;
        write('Bot '), write(CurrentPlayer), write(' has no valid moves!'), nl,
        change_player(CurrentPlayer, NextPlayer),
        game_loop_bot_against_bot((Board, NextPlayer), Difficulty1, Difficulty2)
    ).

% -----------------------------------------------
% Menus
% -----------------------------------------------

% Escolher o modo de jogo
% GameMode = Modo de jogo escolhido
choose_game_mode :-
    nl, write('Choose game mode:'), nl,
    write('1. Player vs Player'), nl,
    write('2. Player vs Bot'), nl,
    write('3. Bot vs Bot'), nl,
    read(GameMode),
    game_mode(GameMode).

% -----------------------------------------------
% Escolhas do menu
% -----------------------------------------------

% Iniciar o jogo modo Player vs Player
play_player_vs_player :-
    initial_board(InitialBoard),
    game_loop((InitialBoard, player1)).

% Iniciar o jogo modo Player vs Bot
play_agaist_bot :-
    write('Choose difficulty:'), nl,
    write('1. Easy'), nl,
    write('2. Hard'), nl, nl,
    read(Difficulty),
    write('Bot difficulty: '), write(Difficulty), nl, nl,
    initial_board(InitialBoard),
    game_loop_against_bot((InitialBoard, player1), Difficulty).

% Iniciar o jogo modo Bot vs Bot
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
% Main
% -----------------------------------------------

% Reiniciar o tabuleiro
reset_board(InitialBoard) :-
    initial_board(InitialBoard).

% Começo do jogo (Função principal)
play :-
    reset_board(InitialBoard),
    menu.

% --------------------------------
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
% Escolhas do menu
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