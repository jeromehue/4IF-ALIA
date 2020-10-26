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


% Create board of 6 lines and 7 columns.
create([  ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-']   ]).



% Display bord.



%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):-    showLine(X,N,X2),
	            Ns is N-1,
	            iShow(X2,Ns).

%showLine(X,N,X2) writes N and shows first line of board X (first element of every column). X2 is X without the shown line.
showLine(X,N,X2):-  write(N), write(' '),
		            iShowLine(X,X2), nl.

%iShowLine(X,X2) writes first element of every column. X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):- write(X), write(' '),
			          iShowLine(XS,XS2).



% Game is over, we cut to stop the search, and display the winner.
%play(_, Board) :- gameover(Winner), !, write('Game is Over. Winner: '), writeln(Winner), show(Board).

% The game is not over, we play the next turn
play(Player, Board) :- write('New turn for:'), writeln(Player),
	show(board(Board)),
	%ia(Board, Move, Player),
	read(Move),
	writeln(Move),
	NewBoard=Board,
	writeln("BOARD OK"),
	playMove(Board, NewBoard, Move, Player, 0),
	writeln("MOVE OK"),
	changePlayer(Player,NextPlayer),
	writeln("CHANGE OK"),
	play(NextPlayer, NewBoard).
	
colonneN([T|_],1,T).
colonneN([_|Q],C,X) :-
    C1 is C-1,
    colonneN(Q,C1,X).
		
playMove(Board, NewBoard, Move, Player, Index) :-
	Index<7, colonneN(Board, Move, Col), nth0(Index, Col, '-'), colonneN(NewBoard, Move, NewCol), nth0(Index, NewCol, Player).
	%;Index<7,NewIndex is Index+1, playMove(Board, NewBoard, Move, Player, NewIndex).
	

changePlayer('X', 'O').
changePlayer('O', 'X').


winnerColonne([Y|B],X):-Y==X,B=[X,X,X|_];winnerColonne(B,X).


extract(ColNumber, Matrix, Column) :- maplist(nth0(ColNumber), Matrix, Column).

winnerLigne1(Board,X,Num):-extract(Num,Board,L1),winnerColonne(L1,X).
winnerLigne2(Board,X):-winnerLigne1(Board,X,0);winnerLigne1(Board,X,1);winnerLigne1(Board,X,2);winnerLigne1(Board,X,3);winnerLigne1(Board,X,4);winnerLigne1(Board,X,5);winnerLigne1(Board,X,6);winnerLigne1(Board,X,7).


%Initialisation
%init :- create(B), play('X', B).
%create(B), colonneN(B,1,C), nth0(1, C, '-'),B=NewBoard,colonneN(NewBoard, 1, NewCol), nth0(0, NewCol, 'a'), show(board(NewBoard)), show(board(B)).
% not working ! But working in morpion ! Why N
