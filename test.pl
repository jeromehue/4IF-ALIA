winner(Board, P) :- Board = [_,[_|[P,Q,R,S]],_],  P==Q, Q==R, R==S, nonvar(P).
winnercol(Col, X) :- Col = [_,X,_], nonvar(X).
winnerColonne([Y|B],X):-Y==X,B=[X,X,X|_];winnerColonne(B,X), nonvar(X).


% Create board of 6 lines and 7 columns.
create(board([  ['-','-','-','-','-','O'],
	            ['-','-','-','-','X','O'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','0'],
	            ['-','-','-','-','-','X'],
	            ['-','-','-','-','-','-'],
	            ['-','-','-','-','-','-']   ])).

init :- 
    create(X),
    write(X).

displayLine :-          create(Board),
                        nth0(0, Board, X),
                        write(X).


element(I, X) :-    maplist(nth0(I),[[a,b,c],[d,e,f],[g,h,i]],R), 
                    nth0(I,R,X).


diagonal :-     element(0, A),
                element(1, B),
                element(2, C),
                X = [A,B,C],
                write(X).

element(I, X, [H|T]) :-     write(T),nl,
                            maplist(nth0(I), [H|T] ,R),
                            write("Found diagonal :"), write(R), nl,
                            element(I, X, T). 
                        


getTail([_|T], T).


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
diagonal_unit_max_7(Board, EI,  7, F) :- write(F).
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
                %write(F),nl,
                % On incrémente l'index de l'élément (EI)
                % et l'index de ligne (LI)
                incr(EI, NEI),
                incr(LI, NLI),
                % et c'est reparti.
                write("parameters : "), write(NEI), write(" , "), write(NLI), nl,
                diagonal_unit_max_7(Board, NEI, NLI, F).

% Renvoie une diagonal avec le 1er élément de la première liste, 
%   le seconde de la seconde, etc...
diag_max_7(Board) :- 
    diagonal_unit_max_7([[a,b,c,d,e,f,g],[a,&,c,d,e,f,g],[h,i,j,k,l,m,n], [a,b,c,d,e,f,g], [a,b,c,d,e,f,g], [a,b,z,d,t,g,h] ], 1, 1, []).        
% Copier/Coller pour tester : 
%diag_max_7([[a,b,c,d,e,f,g],[a,&,c,d,e,f,g],[h,i,j,k,l,m,n], [a,b,c,d,e,f,g], [a,b,c,d,e,f,g], [a,b,z,d,t,g,h] ]).




% Utilitary functions

% Incrementation of a variable
incr(X,X1) :- X1 is X+1.

% Length of a list
nlength([], 0).
nlength([_|Xs] , L) :- nlength(Xs,N) , L is N+1.



 
