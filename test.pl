winner(Board, P) :- Board = [_,[_|[P,Q,R,S]],_],  P==Q, Q==R, R==S, nonvar(P).
winnercol(Col, X) :- Col = [_,X,_], nonvar(X).
winnerColonne([Y|B],X):-Y==X,B=[X,X,X|_];winnerColonne(B,X), nonvar(X).

%%%%%%%%%%%%%%%%%%
% Display board. %
%%%%%%%%%%%%%%%%%%


%show(X) shows board X
show(board(X)):- write('  A B C D E F G'), nl,
		 iShow(X,6).

%show(X,N) shows lines [N .. 1] of board X
iShow(_,0).
iShow(X,N):- 
	showLine(X,N,X2),
    Ns is N-1,
    iShow(X2,Ns).

% showLine(X,N,X2) writes N and shows first line of board X 
% (first element of every column). 
% X2 is X without the shown line.
showLine(X,N,X2):-  
    write(N), write(' '),
	iShowLine(X,X2), nl.

% iShowLine(X,X2) writes first element of every column. 
% X2 is X without the shown line.
iShowLine([],_).
iShowLine([[X|X2]|XS],[X2|XS2]):-
	write(X),
	write(' '),
	iShowLine(XS,XS2).


%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turn columns into rows %
%%%%%%%%%%%%%%%%%%%%%%%%%%

extract(ColNumber, Matrix, Column) :-
    maplist(nth0(ColNumber), Matrix, Column).
    %write(Column).

coltolines(Board, Y) :- 
    extract(0, Board, L1),
    append([],[L1],Y1),
    extract(1, Board, L2),
    append(Y1, [L2], Y2),
    extract(2, Board, L3),
    append(Y2, [L3], Y3),
    extract(3, Board, L4),
    append(Y3, [L4], Y4),
    extract(4, Board, L5),
    append(Y4, [L5], Y5),
    extract(5, Board, L6),
    append(Y5, [L6], Y6),
    write(Y6).
    







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Create board of 6 lines and 7 columns. %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create(board([  ['-','-','-','-','-','O'],
	            ['-','-','-','-','X','O'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','0'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-']   ])).
create2(X) :-  X =  [   ['-','-','-','-','-','O'],
	                    ['-','-','-','-','X','O'],
	                    ['-','-','-','-','-','X'],
	                    ['-','-','0','0','X','0'],
	                    ['-','X','X','X','0','X'],
	                    ['-','-','X','0','0','X'],
	                    ['-','-','a','b','c','d']   ].



displayLine :-  
    create(Board),
    nth0(0, Board, X),
    write(X).


element(I, X) :- 
    maplist(nth0(I),[[a,b,c],[d,e,f],[g,h,i]],R), 
    nth0(I,R,X).


diagonal :- 
    element(0, A),
    element(1, B),
    element(2, C),
    X = [A,B,C],
    write(X).

element(I, X, [H|T]) :-     write(T),nl,
                            maplist(nth0(I), [H|T] ,R),
                            write("Found diagonal :"), write(R), nl,
                            element(I, X, T). 
                        



%%%%%%%%%%%%%%
% Diagonales %
%%%%%%%%%%%%%%


diagonal(Board) :- 
            nth1(1, Board, L1), nth1(2, Board, L2), nth1(3, Board, L3),
            write(L1),nl,
            nth1(1, L1 ,X1),
            write("1 of L1 : "), write(X1), nl,
            nth1(2, L2, X2),
            write("2 of L2 : "), write(X2), nl,
            nth1(3, L3, X3),
            write("3 of L3 : "), write(X3), nl,
            nth1(2, L1, X21),
            write("2 of L1 : "), write(X21), nl,
            nth1(3, L2, X32),
            write("3 of L2 : "), write(X32), nl,
            nth1(3, L1, X31),
            write("3 of L1 : "), write(X31), nl.


diagonal2(Board, EI, LI , FINAL) :-
                            % Get list
                            nth1(LI, Board,  L),
                            nth1(EI, L, X),
                            %write(X),nl,
                            append(FINAL, [X], F),
                            %write(F),nl,
                            incr(EI, NEI),
                            incr(LI, NLI),
                            diagonal2(Board, NEI, NLI, F).

diagonal2(Board, 4, LI, FINAL) :- write(FINAL).

diag3(Board) :- 
                diagonal2(Board, 1, 1, []),
                diagonal2(Board, 2, 1, []),
                diagonal2(Board, 3, 1, []). 





% Mise à l'échelle des fonctions précédentes pour un tableau 6*7.

%   On s'arrêtre lorsque l'on est en bout de ligne 
%   (Car diagonales inférieurs pour le moment)
%   Càd lorsque l'on veut chercher un 7eme élément.
diagonal_unit_max_7(Board, EI,  7, F) :- write(F), nl.
%diagonal_unit_max_7(Board,  8, LI, F) :- write(F).
diagonal_unit_max_7(Board, EI, LI, Final) :-
                % On récupère la ligne
                nth1(LI, Board,  L),
                % On récupère l'élément sur la ligne
                nth1(EI, L, X),
                % Affichage
                %write(X),nl,
                % On ajoute l'élément à notre diagonale
                append(Final, [X], F),
                % Affichage
                write(F),nl,
                % On incrémente l'index de l'élément (EI)
                % et l'index de ligne (LI)
                incr(EI, NEI),
                incr(LI, NLI),
                % et c'est reparti.
                write("par : "), write(NEI), write(" , "), write(NLI), nl,
                diagonal_unit_max_7(Board, NEI, NLI, F).

% Renvoie une diagonal avec le 1er élément de la première liste, 
%   le seconde de la seconde, etc...
diag_max_7(Board) :- 
    diagonal_unit_max_7(Board, 3, 1, []).        
    %diagonal_unit_max_7(Board, 2, 1, Y).        
    %diagonal_unit_max_7(Board, 3, 1, []).        
% Copier/Coller pour tester : 
%diag_max_7([[a,b,c,d,e,f,g],[a,&,c,d,e,f,g],[h,i,j,k,l,m,n], [a,b,c,d,e,f,g], [a,b,c,d,e,f,g], [a,b,z,d,t,g,h] ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Diagonales appliquées au tableau de colonnes %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Un tableau de colonnes est dans notre cas un tableau de 
% 7 listes, chacune composée de 6 éléments.
% Lorsque l'on construit une diagonale, la "condition d'arret"
% est d'être arrivé au dernier éléméent d'une colonne. Soit le 6ème.

upto(Low,High,_Step,Low) :- Low =< High.
upto(Low,High,Step,Var) :-
    Inc is Low+Step,
    Inc =< High,
    upto(Inc, High, Step, Var).

downto(Low,High,_Step,High) :- Low =< High.
downto(Low,High,Step,Var) :-
    Dec is High-Step,
    Dec >= Low,
    downto(Low, Dec, Step, Var).


diagonal_unit(Board, 7, LI, FINAL, R) :- write("AAAAA"), write(FINAL), append([], FINAL, R).
diagonal_unit(Board, EI, LI, FINAL, R) :-
                nth1(LI, Board,  L),% On récupère la colonne.
                nth1(EI, L, X),     % On récupère le bon élément. 
                append(FINAL, [X], F), % Ajout à la diagonale
                incr(EI, NEI),
                incr(LI, NLI),
                diagonal_unit(Board, NEI, NLI, F, R).

diagonal_unit_b(Board, EI, LI, FINAL) :-
                nth1(LI, Board,  L),% On récupère la colonne.
                nth1(EI, L, X),     % On récupère le bon élément. 
                %write(X),nl,
                append(FINAL, [X], F), % Ajout à la diagonale
                %write(F),nl,
                incr(EI, NEI),
                incr(LI, NLI),
                %write("par : "), write(NEI), write(" , "), write(NLI), nl,
                diagonal_unit_b(Board, NEI, NLI, F).
diagonal_unit_b(Board, EI, 8, FINAL) :- write("AAAAA"), write(FINAL), nl.
diagonal_unit_dc(Board, EI, LI, FINAL) :-
                nth1(LI, Board,  L),% On récupère la colonne.
                nth1(EI, L, X),     % On récupère le bon élément. 
                %write(X),nl,
                append(FINAL, [X], F), % Ajout à la diagonale
                %write(F),nl,
                decr(EI, NEI),
                incr(LI, NLI),
                %write("par : "), write(NEI), write(" , "), write(NLI), nl,
                diagonal_unit_dc(Board, NEI, NLI, F).
diagonal_unit_dc(Board, 0, LI, FINAL) :- write("AAAAA"), write(FINAL), nl.
diagonal_unit_dc_b(Board, EI, LI, FINAL) :-
                nth1(LI, Board,  L),% On récupère la colonne.
                nth1(EI, L, X),     % On récupère le bon élément. 
                %write(X),nl,
                append(FINAL, [X], F), % Ajout à la diagonale
                %write(F),nl,
                decr(EI, NEI),
                incr(LI, NLI),
                %write("par : "), write(NEI), write(" , "), write(NLI), nl,
                diagonal_unit_dc_b(Board, NEI, NLI, F).
diagonal_unit_dc_b(Board, EI, 8, FINAL) :- write("AAAAA"), write(FINAL), nl.





diag_final(Board) :- 
    diagonal_unit(Board, 1, 1, []), 
    diagonal_unit(Board, 2, 1, []), 
    diagonal_unit(Board, 3, 1, []), 
    diagonal_unit(Board, 4, 1, []), 
    diagonal_unit(Board, 5, 1, []), 
    diagonal_unit(Board, 6, 1, []),
    diagonal_unit(Board, 1, 2, []),
    diagonal_unit_b(Board, 1, 3, []),
    diagonal_unit_b(Board, 1, 4, []),
    diagonal_unit_b(Board, 1, 5, []),
    diagonal_unit_b(Board, 1, 6, []),
    diagonal_unit_b(Board, 1, 7, []),
    write('Les diagonales dans lautre sens maintenant \n'),
    diagonal_unit_dc(Board, 6,1,[]),
    diagonal_unit_dc(Board, 5,1,[]),
    diagonal_unit_dc(Board, 4,1,[]),
    diagonal_unit_dc(Board, 3,1,[]),
    diagonal_unit_dc(Board, 2,1,[]),
    diagonal_unit_dc(Board, 1,1,[]),
    diagonal_unit_dc_b(Board, 6,2,[]),
    diagonal_unit_dc_b(Board, 6,3,[]),
    diagonal_unit_dc_b(Board, 6,4,[]),
    diagonal_unit_dc_b(Board, 6,5,[]),
    diagonal_unit_dc_b(Board, 6,6,[]),
    diagonal_unit_dc_b(Board, 6,7,[]).

diagtestf :- 
    create2(X),
    write(board(X)),nl,
    show(board(X)),
    diag_final(X).

buildDiag(Board, X) :-
    forall(upto(0,6,1,V),write(V), nth0()).

getDiag() :-
    create2(X),
    write(X),nl,
    diagonal_unit(X, 1,1,  [],A),write(A).
    %forall(upto(0,6,1,V),diagonal_unit(X,V,V,A)).
%%%%%%%%%%%%%%%%%%%%%%%
% Utilitary functions %
%%%%%%%%%%%%%%%%%%%%%%%

% Incrementation of a variable
incr(X,X1) :- X1 is X+1.

% Decrementation of a variable
decr(X, X1) :- X1 is X-1.

% Length of a list
nlength([], 0).
nlength([_|Xs] , L) :- nlength(Xs,N) , L is N+1.

% Create a board and display it.


outputs :- write("Bonjour").
 
init :- 
    create2(X),
    show(board(X)),
    coltolines(X,Y).

getTail([_|T], T).
