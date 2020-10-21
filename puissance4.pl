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
create(board([  ['-','-','-','-','-','O'],
	            ['-','-','-','-','X','O'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','0'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-']   ])).



% Display bord.



%show(X) shows boarddd X
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



% Play a move
play(Player) :- write('New turn for player : '), writeln(Player),
                A is random(7),
                write('Playing in the column n° : '), writeln(A).

% Jouer un coup. 


%Initialisation
init :- create(X), show(X).


