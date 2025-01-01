% -----------------------------------------------
% Bot Moves
% -----------------------------------------------

% Escolhe um elemento aleatório de uma lista
% List = Lista de elementos
% Element = Elemento escolhido aleatoriamente
random_list_move_member(List, Element) :-
    length(List, Length),  
    Length > 0, 
    random(0, Length, Index),  
    nth0(Index, List, Element).  


% Escolhe uma peça aleatória e realiza um movimento aleatório entre os possíveis (Bot Easy)
% Board = Tabuleiro atual
% SrcX-SrcY = Posição de origem
% DestX-DestY = Posição de destino
% Player = Jogador atual
% 1 = Dificuldade
% NewBoard = Novo tabuleiro após o movimento
choose_move_with_bot(Board, SrcX-SrcY, DestX-DestY, Player, 1, NewBoard) :- 
    valid_moves_list(Board, Player, MovePairs),
    MovePairs \= [],  
    findall(SrcX-SrcY, member(SrcX-SrcY-_-_, MovePairs), SrcListDup),
    sort(SrcListDup, SrcList),  
    random_member(SrcX-SrcY, SrcList),
    findall(DestX-DestY, member(SrcX-SrcY-DestX-DestY, MovePairs), DestList),
    random_member(DestX-DestY, DestList),
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).


% Escolhe um movimento capturante se possível, caso contrário, escolhe um movimento não capturante aleatório (Bot Hard)
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
        random_member(SrcX-SrcY-DestX-DestY, NonCapturingMoves)
    ),
    piece(Player, Piece),
    put_piece(Board, SrcX-SrcY, empty, TempBoard),
    put_piece(TempBoard, DestX-DestY, Piece, NewBoard),
    format('Bot move: (~w, ~w) -> (~w, ~w)~n', [SrcX, SrcY, DestX, DestY]).
