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

