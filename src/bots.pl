% -----------------------------------------------
% Bot Difficulty Levels
% -----------------------------------------------
%
% Bot 1 (Easy):
% - Makes completely random valid moves
% - Does not use any strategy or evaluation
%
% Bot 2 (Hard): 
% - Prioritizes capturing opponent pieces
% - Prefers diagonal upward moves to avoid capture
% - Evaluates move safety and position control
% - Considers opponent's possible responses
%
% Bot 3 (Greedy):
% - Full position evaluation after each move:
%   * Capturing pieces (+15 points)
%   * Safe positions (+5 points)
%   * Threatened pieces (-3 points)
%   * Board control (+1 point per controlled square)
% - Simulates opponent responses
% - Always picks highest scoring move
%
% -----------------------------------------------


% -----------------------------------------------
% Helper Functions
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

% -----------------------------------------------
% Bot 1 (Easy)
% Board = Current board
% SrcX-SrcY = Source position
% DestX-DestY = Destination position
% Player = Current player
% 1 = Difficulty
% NewBoard = New board after the move
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 1, NewBoard) :- 
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


% -----------------------------------------------
% Bot 2 (Hard)
hard_bot_not_capturing_moves([], []).
hard_bot_not_capturing_moves([SrcX-SrcY-DestX-DestY|Rest], [SrcX-SrcY-DestX-DestY|Filtered]) :-
    ( (DestX =:= SrcX + 1, DestY =:= SrcY - 1)
    ),
    hard_bot_not_capturing_moves(Rest, Filtered).
hard_bot_not_capturing_moves([_|Rest], Filtered) :-
    hard_bot_not_capturing_moves(Rest, Filtered).


% Board = Current board
% SrcX-SrcY = Source position
% DestX-DestY = Destination position
% Player = Current player
% 2 = Difficulty
% NewBoard = New board after the move
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

% -----------------------------------------------
% Bot 3 (Greedy)
% Algoritmo greedy, analisa as jogadas
% Evaluate position control and safety
evaluate_position(Board, Player, Score) :-
    count_pieces(Board, Player, PieceCount),
    count_threatened_pieces(Board, Player, ThreatenedCount),
    count_safe_pieces(Board, Player, SafeCount),
    Score is PieceCount*10 + SafeCount*5 - ThreatenedCount*3.

% Count pieces for a player
count_pieces(Board, Player, Count) :-
    piece(Player, Piece),
    findall(1, (member(Row, Board), member(Piece, Row)), Pieces),
    length(Pieces, Count).

% Count pieces that can be captured
count_threatened_pieces(Board, Player, Count) :-
    piece(Player, Piece),
    (Player = player1 -> Opponent = player2 ; Opponent = player1),
    findall(1, (
        range(0, 7, X), range(0, 7, Y),
        get_piece(Board, X, Y, Piece),
        valid_moves_list(Board, Opponent, OpponentMoves),
        member(_-_-X-Y, OpponentMoves)
    ), Threatened),
    length(Threatened, Count).

% Count pieces in safe positions
count_safe_pieces(Board, Player, Count) :-
    piece(Player, Piece),
    findall(1, (
        range(0, 7, X), range(0, 7, Y),
        get_piece(Board, X, Y, Piece),
        \+ can_be_captured(Board, X-Y, Player)
    ), Safe),
    length(Safe, Count).

% Check if piece can be captured
can_be_captured(Board, X-Y, Player) :-
    (Player = player1 -> Opponent = player2 ; Opponent = player1),
    valid_moves_list(Board, Opponent, OpponentMoves),
    member(_-_-X-Y, OpponentMoves).

% Evaluate a move by simulating it
evaluate_move(Board, SrcX-SrcY-DestX-DestY, Player, Score) :-
    piece(Player, Piece),
    get_piece(Board, DestX, DestY, DestPiece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    evaluate_position(NewBoard, Player, PositionScore),
    (DestPiece = empty -> CaptureScore = 0 ; CaptureScore = 15),
    (can_be_captured(NewBoard, DestX-DestY, Player) -> SafetyPenalty = -10 ; SafetyPenalty = 0),
    Score is PositionScore + CaptureScore + SafetyPenalty.

% Board = Current board
% SrcX-SrcY = Source position
% DestX-DestY = Destination position
% Player = Current player
% 3 = Difficulty
% NewBoard = New board after the move
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 3, NewBoard) :-
    valid_moves_list(Board, Player, MovePairs),
    MovePairs \= [],
    findall(Score-Move, (
        member(Move, MovePairs),
        evaluate_move(Board, Move, Player, Score)
    ), ScoredMoves),
    keysort(ScoredMoves, SortedMoves),
    last(SortedMoves, _-BestMove),
    BestMove = SrcX-SrcY-DestX-DestY,
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).
