% Verifica se a posição está dentro do tabuleiro
within_board(X, Y) :- 
    X >= 0, X =< 7,
    Y >= 0, Y =< 7.

% -----------------------------------------------
% Funções para verificar se o caminho está livre
% -----------------------------------------------
% -----------------------------------------------
% Jogador Branco
% -----------------------------------------------

% free_path_white para a Direção 1 (Para baixo)
% Verifica se o caminho está livre para o jogador branco na direção 1 (para baixo)
free_path_white(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    (
        NextY = DestY
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, X, NextY, empty),
        free_path_white(Board, X-NextY, X-DestY, 1)
    ).

% free_path_white para a Direção 2 (Diagonal baixo-esquerda)
% Verifica se o caminho está livre para o jogador branco na direção 2 (diagonal baixo-esquerda)
free_path_white(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_white(Board, NextX-NextY, DestX-DestY, 2)
    ).

% free_path_white para a Direção 3 (Para a esquerda)
% Verifica se o caminho está livre para o jogador branco na direção 3 (para a esquerda)
free_path_white(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    (
        NextX = DestX
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, Y, empty),
        free_path_white(Board, NextX-Y, DestX-Y, 3)
    ).

% free_path_white para a Direção 4 (Diagonal cima-esquerda)
% Verifica se o caminho está livre para o jogador branco na direção 4 (diagonal cima-esquerda)
free_path_white(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_white(Board, NextX-NextY, DestX-DestY, 4)
    ).

% -----------------------------------------------
% Jogador Preto
% -----------------------------------------------

% free_path_black para a Direção 1 (Para baixo)
% Verifica se o caminho está livre para o jogador preto na direção 1 (para baixo)
free_path_black(Board, X-Y, X-DestY, 1) :-
    Y < DestY,
    NextY is Y + 1,
    within_board(X, NextY),
    (
        NextY = DestY
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, X, NextY, empty),
        free_path_black(Board, X-NextY, X-DestY, 1)
    ).

% free_path_black para a Direção 2 (Diagonal baixo-esquerda)
% Verifica se o caminho está livre para o jogador preto na direção 2 (diagonal baixo-esquerda)
free_path_black(Board, X-Y, DestX-DestY, 2) :-
    X > DestX,
    Y < DestY,
    NextX is X - 1,
    NextY is Y + 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_black(Board, NextX-NextY, DestX-DestY, 2)
    ).

% free_path_black para a Direção 3 (Para a esquerda)
% Verifica se o caminho está livre para o jogador preto na direção 3 (para a esquerda)
free_path_black(Board, X-Y, DestX-Y, 3) :-
    X > DestX,
    NextX is X - 1,
    within_board(NextX, Y),
    (
        NextX = DestX
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, Y, empty),
        free_path_black(Board, NextX-Y, DestX-Y, 3)
    ).

% free_path_black para a Direção 4 (Diagonal cima-esquerda)
% Verifica se o caminho está livre para o jogador preto na direção 4 (diagonal cima-esquerda)
free_path_black(Board, X-Y, DestX-DestY, 4) :-
    X > DestX,
    Y > DestY,
    NextX is X - 1,
    NextY is Y - 1,
    within_board(NextX, NextY),
    (
        (NextX = DestX, NextY = DestY)
    ->  true  % Destino alcançado; não requer que esteja vazio; captura a peça adversária
    ;   get_piece(Board, NextX, NextY, empty),
        free_path_black(Board, NextX-NextY, DestX-DestY, 4)
    ).

% -----------------------------------------------
% Movement Rules
% -----------------------------------------------

% -----------------------------------------------
% Jogador Branco
% -----------------------------------------------

% Movimentos não capturantes para o jogador branco (player1)
% Board = Tabuleiro atual
% X-Y = Posição de origem
% Nx-Ny = Posição de destino
% player1 = Jogador atual
valid_move(Board, X-Y, Nx-Ny, player1) :- 
    get_piece(Board, X, Y, w),
    (
      % Movimentos não capturantes (1 casa)
      (Nx is X, Ny is Y - 1)      % Para cima
    ; (Nx is X + 1, Ny is Y - 1)  % Para diagonal cima-direita
    ; (Nx is X + 1, Ny is Y)      % Para a direita
    ; (Nx is X + 1, Ny is Y + 1)  % Para diagonal baixo-direita
    ),
    get_piece(Board, Nx, Ny, empty), !.

% Movimentos capturantes para o jogador branco (player1)
% Board = Tabuleiro atual
% X-Y = Posição de origem
% Nx-Ny = Posição de destino
% player1 = Jogador atual
valid_move(Board, X-Y, Nx-Ny, player1) :- 
    get_piece(Board, X, Y, w),
    (
      (Nx = X, Ny > Y), Direction = 1        % Para baixo
    ; (Nx < X, Ny > Y), Direction = 2        % Diagonal baixo-esquerda
    ; (Nx < X, Ny = Y), Direction = 3        % Para a esquerda
    ; (Nx < X, Ny < Y), Direction = 4        % Diagonal cima-esquerda
    ),
    free_path_white(Board, X-Y, Nx-Ny, Direction),
    % Movimentos capturantes para o jogador branco (+ de 1 casa, usar o free_path_white)
    get_piece(Board, Nx, Ny, b), !.

% -----------------------------------------------
% Jogador Preto
% -----------------------------------------------

% Movimentos não capturantes para o jogador preto (player2)
% Board = Tabuleiro atual
% X-Y = Posição de origem
% Nx-Ny = Posição de destino
% player1 = Jogador atual
valid_move(Board, X-Y, Nx-Ny, player2) :- 
    get_piece(Board, X, Y, b),
    (
        % Movimentos não capturantes (1 casa)
        (Nx is X, Ny is Y - 1)    % Para cima
    ;   (Nx is X + 1, Ny is Y - 1)  % Diagonal cima-direita
    ;   (Nx is X - 1, Ny is Y - 1)  % Diagonal cima-esquerda
    ;   (Nx is X + 1, Ny is Y)      % Para a direita
    ),
    get_piece(Board, Nx, Ny, empty), !.

% Movimentos capturantes para o jogador preto (player2)
% Board = Tabuleiro atual
% X-Y = Posição de origem
% Nx-Ny = Posição de destino
% player1 = Jogador atual
valid_move(Board, X-Y, Nx-Ny, player2) :- 
    get_piece(Board, X, Y, b),
    (
      (Nx = X, Ny > Y) , Direction = 1      % Para baixo
    ; (Nx < X, Ny > Y) , Direction = 2      % Diagonal baixo-esquerda
    ; (Nx < X, Ny = Y) , Direction = 3      % Para a esquerda
    ; (Nx > X, Ny > Y) , Direction = 4      % Diagonal baixo-direita
    ),
    % Movimentos capturantes (+ de 1 casa, usar o free_path_black)
    free_path_black(Board, X-Y, Nx-Ny, Direction),
    get_piece(Board, Nx, Ny, w), !.
