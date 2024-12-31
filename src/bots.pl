% -----------------------------------------------
% Bot Moves
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

% Escolhe uma peça aleatória e realiza um movimento aleatório entre os possíveis (Easy)
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 1, NewBoard) :- % Difficulty 1 is Easy
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
    % Exibe o movimento realizado pelo bot
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).


% Escolhe um movimento aleatório entre os possíveis (Medium)
% Board = Tabuleiro atual
% SrcX-SrcY = Posição de origem
% DestX-DestY = Posição de destino
% Player = Jogador atual
% 2 = Dificuldade
% NewBoard = Novo tabuleiro após o movimento
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 2, NewBoard) :- % Difficulty 2 is Hard
    valid_moves_list(Board, Player, MovePairs),
    MovePairs \= [],  % Ensure there are available moves
    % Separate capturing and non-capturing moves
    (Player = player1 -> OpponentPiece = b ; OpponentPiece = w),
    findall(SrcX-SrcY-DestX-DestY, (member(SrcX-SrcY-DestX-DestY, MovePairs), get_piece(Board, DestX, DestY, OpponentPiece)), CapturingMoves),
    findall(SrcX-SrcY-DestX-DestY, (member(SrcX-SrcY-DestX-DestY, MovePairs), get_piece(Board, DestX, DestY, empty)), NonCapturingMoves),
    % Prioritize capturing moves
    (CapturingMoves \= [] ->
        random_member(SrcX-SrcY-DestX-DestY, CapturingMoves)
    ;
        random_member(SrcX-SrcY-DestX-DestY, NonCapturingMoves)
    ),
    % Perform the move on the board
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    % Display the move made by the bot
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).
