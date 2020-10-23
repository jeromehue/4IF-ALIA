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
                            write(X),nl,
                            append(FINAL, [X], F),
                            write(F),nl,
                            incr(EI, NEI),
                            incr(LI, NLI),
                            diagonal2(Board, NEI, NLI, F).

diagonal2(Board, 3, LI, FINAL) :- write("FIN\n").

diag3(Board) :- 
                diagonal2(Board, 1, 1, []),
                diagonal2(Board, 2, 1, []),
                diagonal2(Board, 3, 1, []). 

incr(X,X1) :- X1 is X+1. 
