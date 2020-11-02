%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    ALIA - Puissance 4 - Hexanôme 4414     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Schématisation : 
%   - emplacement vide
%   O pion de l’adversaire
%   X pion du joueur

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Création du plateau
% à 6 lignes et 7 colonnes

board([
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ],
    [ '-',  '-',  '-',  '-',  '-', '-' ]
]).
:- dynamic board/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Vérifications de fin de jeu

winnerColonne([Y|B], X) :-
    Y == X,
    B = [X,X,X|_];
    winnerColonne(B, X).

winnerLigne1(Board, X, Num):-
    extract(Num, Board, L1),
    winnerColonne(L1, X).

winnerLigne2(Board, X) :-
    winnerLigne1(Board, X, 0);
    winnerLigne1(Board, X, 1);
    winnerLigne1(Board, X, 2);
    winnerLigne1(Board, X, 3);
    winnerLigne1(Board, X, 4);
    winnerLigne1(Board, X, 5);
    winnerLigne1(Board, X, 6);
    winnerLigne1(Board, X, 7).

checkDraw(_, 7) :- writeln('Égalité!'), halt.
checkDraw(Board, Index) :-
    nth0(Index, Board, Column),
    not(member('-', Column)),
    NewIndex is Index+1,
    checkDraw(Board, NewIndex).

checkall(Board, Player) :-
    nth0(0, Board, C), winnerColonne(C, Player);
    nth0(1, Board, C), winnerColonne(C, Player);
    nth0(2, Board, C), winnerColonne(C, Player);
    nth0(3, Board, C), winnerColonne(C, Player);
    nth0(4, Board, C), winnerColonne(C, Player);
    nth0(5, Board, C), winnerColonne(C, Player);
    nth0(6, Board, C), winnerColonne(C, Player);
    winnerLigne2(Board, Player),
    % Vérifier les diagonales (essais dans test.pl)
    checkDraw(Board, 0).

win(Board) :-
    checkall(Board, 'X'),
	write('X a gagné !'), nl, write('FIN'), nl,
    halt;
	checkall(Board, 'O'),
	write('O a gagné !'), nl, write('FIN'), nl,
    halt.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Affichage du plateau

display(_, 7, _).
display(N, I, Board) :- 
    nth0(I, Board, Col), nth0(N, Col, Cell),
    write(Cell), write('  '), J is I+1, display(N, J, Board).

displayBoard(Board) :-
    nl, writeln(' A  B  C  D  E  F  G'),
    write(' '), display(5, 0, Board), nl,
    write(' '), display(4, 0, Board), nl,
    write(' '), display(3, 0, Board), nl,
    write(' '), display(2, 0, Board), nl,
    write(' '), display(1, 0, Board), nl,
    write(' '), display(0, 0, Board), nl,
    not(win(Board)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation des joueurs

humanOrAI([P1|P2]):- 
	writeln('Player 1, (H)uman or (C)omputer :'),
	get_char(P1),
	get_code(_),
	writeln('Player 2, (H)uman or (C)omputer :'),
	get_char(P2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA

% Jouer aléatoirement
ia(Board, Move, _) :-
	random_between(0, 6, PossibleMove),
	(
        not(isValidMove(PossibleMove, Board)) -> ia(Board, Move, _)
	    ; Move is PossibleMove
	).

% Prioriser les coups offrants
% le plus de possibilités d’alignement
ia1(_, _, _).

% Prioriser les coups offrants
% le plus de possibilités d’alignement
% et réduisants ceux de l’adversaire
ia2(_, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Déroulement du jeu

% The game is not over, we play the next turn
play(Player, Board, Human) :- 
	displayBoard(Board),
	write('Au tour de : '), writeln(Player),
    currentMove(Human, Player, Move, Board),
	indexForMove(Board, Move, Index),
    playMove(Board, NewBoard, Move, Player, Index),
	changePlayer(Player, NextPlayer),
	play(NextPlayer, NewBoard, Human).

% returns the move to be played and the corresponding column index
% if move is invalid, it loops
currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 'H',
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(PossibleMove),
	(  not(isValidMove(PossibleMove, Board)) -> currentMove([P1|_], Player, Move, Board)
	;   Move is PossibleMove
	).
	
currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 'H',
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(PossibleMove),
	( not(isValidMove(PossibleMove, Board)) -> currentMove([_|P2], Player, Move, Board)
	; Move is PossibleMove
	).

currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 'C',
	ia(Board, Move, Player).
	
currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 'C',
	ia(Board, Move, Player).

% TBD
isValidMove(Move, Board) :-
	Move >= 0,
    Move < 7,
    indexForMove(Board, Move, Index),
    Index < 6,
    Index >= 0.

% returns the index of first empty space for a move
indexForMove(Board, Move, Index) :-
	colonneN(Board, Move, Col),
    indexForColumn(Col, 0, Index).

indexForColumn([T|_], Index, Index) :- T = '-'.
indexForColumn([_|H], Index, NewIndex) :-
    Tmp = Index+1,
    indexForColumn(H, Tmp, NewIndex).
	
playMove(Board, NewBoard, Move, Player, Index) :-
	Index < 7,
    colonneN(Board, Move, Col),
    applyMoveColumn(Col, NewCol, Index, Player),
    applyMoveBoard(Board, NewBoard, Col, NewCol, Move).
	% ;Index<7, NewIndex is Index+1, playMove(Board, NewBoard, Move, Player, NewIndex).

applyMoveColumn(Col, NewCol, Index, Player) :-
	take(Index, Col, T),
    append(T, ['-'|H], Col),
    append(T, [Player|H], NewCol).
	% take(Index, Col, T), length(T, Index), H is ['-'|Q], H2 is [Player|Q].
	
applyMoveBoard(Board, NewBoard, Col, NewCol, Move) :-
	take(Move, Board, T),
    append(T, [Col|H], Board),
    append(T, [NewCol|H], NewBoard).
    % write(T), nl, write(NewBoard), nl.
	% length(T, Move), H is [Col|Q], H2 is [NewCol|Q].

changePlayer('X', 'O').
changePlayer('O', 'X').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation du jeu

init :-
    board(Board),             % Récupération du plateau de jeu
    humanOrAI(Player),        % Initialisation des joueurs
    play('X', Board, Player). % Début du jeu

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prédicats utiles

% Take the first N elements from List and unify this with FirstElements. 
% The definition is based on the GNU-Prolog lists library. 
% Implementation by Jan Wielemaker.
take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).
	
% colonneN(Board, n, Col) returns n-th column of board in Col
colonneN([T|_], 0, T).
colonneN([_|Q], C, X) :-
    C1 is C-1,
    colonneN(Q, C1, X).

premierElement([X]) :- write(X).
premierElement([H|T]) :- 
	nl,
	write(H),
 	premierElement(T).

extract(ColNumber, Matrix, Column) :-
    maplist(nth0(ColNumber), Matrix, Column).
    % write(Column).
