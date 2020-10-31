%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ALIA - Puissance 4 - Hexanôme 4414 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Schématisation : 
%   - emplacement vide inaccessible
%   ? emplacement vide accessible
%   o pion de l’adversaire
%   x pion du joueur
%   ! emplacement qui permet de faire un puissance 4

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Création du plateau
% à 6 lignes et 7 colonnes

board([
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ],
    [ '-',  '-',  '-',  '-',  '-', '?' ]
]).
:- dynamic board/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Affichage

display(_, -1).
display(N, I) :- 
    board(Board),
    nth0(I, Board, Col),
    nth0(N, Col, Cell),
    write(Cell), write('  '),
    J is I-1,
    display(N, J).

displayBoard :- 
    nl, writeln(' A  B  C  D  E  F  G'),
    write(' '), display(0, 6), nl,
    write(' '), display(1, 6), nl,
    write(' '), display(2, 6), nl,
    write(' '), display(3, 6), nl,
    write(' '), display(4, 6), nl,
    write(' '), display(5, 6), nl.

display(_, -1, _).
display(N, I, Board) :- 
    nth0(I, Board, Col),nth0(N, Col, Cell),
    write(Cell), write('  '),J is I-1,display(N, J, Board).

% Display the board that is given
displayBoard(Board) :-
    %write("Plain text"), write(Board), nl,
    nl, writeln(' A  B  C  D  E  F  G'),
    write(' '), display(0, 6, Board), nl,
    write(' '), display(1, 6, Board), nl,
    write(' '), display(2, 6, Board), nl,
    write(' '), display(3, 6, Board), nl,
    write(' '), display(4, 6, Board), nl,
    write(' '), display(5, 6, Board), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation des joueurs

humanOrAI([P1|P2]):- 
	writeln('Taper H si le joueur 1 est humain, C sinon :'),
	get_char(P1),
	get_code(_),
	writeln('Taper H si le joueur 2 est humain, C sinon :'),
	get_char(P2).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Jeu (à améliorer)

% Game is over, we cut to stop the search, and display the winner.
% TBD
%play(_, Board) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), show(Board).

% The game is not over, we play the next turn
play(Player, Board, Human) :- 
	write('Au tour de : '), writeln(Player),
    %displayBoard,
	displayBoard(Board),
    currentMove(Human, Player, Move, Board),
	writeln(Move),
%	writeln('BOARD OK'),
%	isValidMove(Board, Move),
	indexForMove(Board, Move, Index),
	playMove(Board, NewBoard, Move, Player, Index),
%	writeln('MOVE OK'),
	changePlayer(Player,NextPlayer),
%	writeln('CHANGE OK'),
	play(NextPlayer, NewBoard, Human).

% returns the move to be played
currentMove([P1|_], Player, Move, _) :-
	Player == 'X',
	P1 == 'H',
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(Move).

currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 'C',
	ia(Board, Move, Player).
	
currentMove([_|P2], Player, Move, _) :-
	Player == 'O',
	P2 == 'H',
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(Move).
	
currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 'C',
	ia(Board, Move, Player).
	
% stupid random IA not even checking if move is correct
ia(_, Move, _) :-
	random_between(0, 6, Move).

% TBD
%isValidMove() :-

% returns the index of first empty space for a move
indexForMove(Board, Move, Index) :-
	Move<7, colonneN(Board, Move, Col), indexForColumn(Col, 0, Index).

indexForColumn([T|_], Index, Index) :- T = '-'.
indexForColumn([_|H], Index, NewIndex) :- Tmp = Index+1, indexForColumn(H, Tmp, NewIndex).
	
playMove(Board, NewBoard, Move, Player, Index) :-
	Index<7, colonneN(Board, Move, Col), applyMoveColumn(Col, NewCol, Index, Player), applyMoveBoard(Board, NewBoard, Col, NewCol, Move).
	%;Index<7,NewIndex is Index+1, playMove(Board, NewBoard, Move, Player, NewIndex).

applyMoveColumn(Col, NewCol, Index, Player) :-
	take(Index, Col, T), append(T, ['-'|H], Col), append(T, [Player|H], NewCol).
	%take(Index, Col, T), length(T, Index), H is ['-'|Q], H2 is [Player|Q].
	
applyMoveBoard(Board, NewBoard, Col, NewCol, Move) :-
	take(Move, Board, T), append(T, [Col|H], Board), append(T, [NewCol|H], NewBoard).
%   write(T), nl, write(NewBoard), nl.
	%length(T, Move), H is [Col|Q], H2 is [NewCol|Q].

changePlayer('X', 'O').
changePlayer('O', 'X').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Initialisation du jeu

init :-
    board(Board),               % Récupération du plateau de jeu
    humanOrAI(Player),          % Initialisation des joueurs
    play('X', Board, Player).   % Début du jeu

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
colonneN([T|_],0,T).
colonneN([_|Q],C,X) :-
    C1 is C-1,
    colonneN(Q,C1,X).

premierElement([X]) :- write(X).
premierElement([H|T]) :- 
	nl,
	write(H),
 	premierElement(T).

extract(ColNumber, Matrix, Column) :-
    maplist(nth0(ColNumber), Matrix, Column),
    write(Column).
	
% Colonne is winning
winnerColonne([Y|B],X):-Y==X,B=[X,X,X|_];winnerColonne(B,X).

% Line is winning
winnerLigne1(Board,X,Num):-extract(Num,Board,L1),winnerColonne(L1,X).
winnerLigne2(Board,X):-winnerLigne1(Board,X,0);winnerLigne1(Board,X,1);winnerLigne1(Board,X,2);winnerLigne1(Board,X,3);winnerLigne1(Board,X,4);winnerLigne1(Board,X,5);winnerLigne1(Board,X,6);winnerLigne1(Board,X,7).
