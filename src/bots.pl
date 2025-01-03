% -----------------------------------------------
% Bot Moves
% -----------------------------------------------

% Prioriza movimentos de captura e movimentos na diagonal para cima direita (dificil de ser capturado) BOT NIVEL 2
% SrcX-SrcY-DestX-DestY = Movimento a ser realizado
hard_bot_not_capturing_moves([], []).
hard_bot_not_capturing_moves([SrcX-SrcY-DestX-DestY|Rest], [SrcX-SrcY-DestX-DestY|Filtered]) :-
    ( (DestX =:= SrcX + 1, DestY =:= SrcY - 1)
    ),
    hard_bot_not_capturing_moves(Rest, Filtered).
hard_bot_not_capturing_moves([_|Rest], Filtered) :-
    hard_bot_not_capturing_moves(Rest, Filtered).


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
    MovePairs \= [],  
    findall(SrcX-SrcY, member(SrcX-SrcY-_-_, MovePairs), SrcListDup),
    sort(SrcListDup, SrcList),  
    random_member(SrcX-SrcY, SrcList),
    findall(DestX-DestY, member(SrcX-SrcY-DestX-DestY, MovePairs), DestList),
    random_member(DestX-DestY, DestList),
    % Realiza o movimento no tabuleiro
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    % Exibe o movimento realizado pelo bot
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).


% Prioriza movimentos de captura e movimentos na diagonal para cima direita (dificil de ser capturado) BOT NIVEL 2
% Board = Tabuleiro atual
% SrcX-SrcY = Posição de origem
% DestX-DestY = Posição de destino
% Player = Jogador atual
% 2 = Dificuldade
% NewBoard = Novo tabuleiro após o movimento
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 2, NewBoard) :-
    valid_moves_list(Board, Player, MovePairs),
    MovePairs \= [],
    (Player = player1 -> OpponentPiece = b ; OpponentPiece = w),
    findall(SrcX-SrcY-DestX-DestY, (member(SrcX-SrcY-DestX-DestY, MovePairs), get_piece(Board, DestX, DestY, OpponentPiece)), CapturingMoves),
    findall(SrcX-SrcY-DestX-DestY, (member(SrcX-SrcY-DestX-DestY, MovePairs), get_piece(Board, DestX, DestY, empty)), NonCapturingMoves),
    (CapturingMoves \= [] ->
        random_member(SrcX-SrcY-DestX-DestY, CapturingMoves)
    ;
        hard_bot_not_capturing_moves(NonCapturingMoves, PriorityMoves),
        (PriorityMoves \= [] ->
            random_member(SrcX-SrcY-DestX-DestY, PriorityMoves)
        ;
            random_member(SrcX-SrcY-DestX-DestY, NonCapturingMoves)
        )
    ),
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).



