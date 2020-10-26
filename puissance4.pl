% The game state is represented by a 2D array of 42 elements
%   board ([['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-'],
%	        ['-','-','-','-','-','-']])) at the beginning.
%   
%   Schématisation : 
%   - emplacement vide inaccessible
%   ? emplacement vide accessible
%   o pion de l’adversaire
%   x pion du joueur
%   ! emplacement qui permet de faire un puissance 4
%

:- dynamic board/1.


% Create board of 6 lines and 7 columns.
create(board([  ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-']   ])).



% Display bord.



%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- 
	showLine(X,N,X2),
    Ns is N-1,
    iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):-  write(N), write(' '),
		            iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):-
	write(X),
	write(' '),
	iShowLine(XS,XS2).


% Game is over, we cut to stop the search, and display the winner.
% TBD
%play(_, Board) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), show(Board).

% The game is not over, we play the next turn
play(Player, Board) :- write('New turn for : '), writeln(Player),
	show(board(Board)),
	%ia(Board, Move, Player),
	read(Move),
	writeln(Move),
%	writeln("BOARD OK"),
	%isValidMove(Board, Move),
	getIndexForMove(Board, Move, Index),
	playMove(Board, NewBoard, Move, Player, Index),
%	writeln("MOVE OK"),
	changePlayer(Player,NextPlayer),
%	writeln("CHANGE OK"),
	play(NextPlayer, NewBoard).

% colonneN(Board, n, Col) returns n-th column of board in Col
colonneN([T|_],0,T).
colonneN([_|Q],C,X) :-
    C1 is C-1,
    colonneN(Q,C1,X).

% TBD
%isValidMove() :-

% returns the index of first empty space for a move
getIndexForMove(Board, Move, Index) :-
	Move<7, colonneN(Board, Move, Col), getIndexForColumn(Col, 0, Index).

getIndexForColumn([T|_], Index, Index) :- T = '-'.
getIndexForColumn([_|H], Index, NewIndex) :- Tmp = Index+1, getIndexForColumn(H, Tmp, NewIndex).
	
	

playMove(Board, NewBoard, Move, Player, Index) :-
	Index<7, colonneN(Board, Move, Col), applyMoveColumn(Col, NewCol, Index, Player), applyMoveBoard(Board, NewBoard, Col, NewCol, Move).
	%;Index<7,NewIndex is Index+1, playMove(Board, NewBoard, Move, Player, NewIndex).

applyMoveColumn(Col, NewCol, Index, Player) :-
	take(Index, Col, T), append(T, ['-'|H], Col), append(T, [Player|H], NewCol).
	%take(Index, Col, T), length(T, Index), H is ['-'|Q], H2 is [Player|Q].
	
applyMoveBoard(Board, NewBoard, Col, NewCol, Move) :-
	take(Move, Board, T), append(T, [Col|H], Board), append(T, [NewCol|H], NewBoard).
	%length(T, Move), H is [Col|Q], H2 is [NewCol|Q].

changePlayer('X', 'O').
changePlayer('O', 'X').

%Initialisation
init :- create(board(B)), play('X', B).


% useful stuff

%Take the first N elements from List and unify this with FirstElements. 
%The definition is based on the GNU-Prolog lists library. 
%Implementation by Jan Wielemaker.
take(0, _, []) :- !.
take(N, [H|TA], [H|TB]) :-
	N > 0,
	N2 is N - 1,
	take(N2, TA, TB).
	

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
