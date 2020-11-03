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

%Les diagonales
diagonal_unit(_, 7, _, FINAL, R) :- append([], FINAL, R).
diagonal_unit(Board, EI, LI, FINAL, R) :-
    nth1(LI, Board,  L),% On récupère la colonne.
    nth1(EI, L, X),     % On récupère le bon élément.
    %write(X),nl,
    append(FINAL, [X], F), % Ajout à la diagonale
    incr(EI, NEI),
    incr(LI, NLI),
    %write("par : "), write(NEI), write(" , "), write(NLI), nl,
    diagonal_unit(Board, NEI, NLI, F, R).

diagonal_unit_b(_, _, 8, FINAL, R) :- append([], FINAL, R).
diagonal_unit_b(Board, EI, LI, FINAL, R) :-
    nth1(LI, Board,  L),% On récupère la colonne.
    nth1(EI, L, X),     % On récupère le bon élément.
    %write(X),nl,
    append(FINAL, [X], F), % Ajout à la diagonale
    %write(F),nl,
    incr(EI, NEI),
    incr(LI, NLI),
    %write("par : "), write(NEI), write(" , "), write(NLI), nl,
    diagonal_unit_b(Board, NEI, NLI, F, R).

diagonal_unit_dc(Board, EI, LI, FINAL, R) :-
    nth1(LI, Board,  L),% On récupère la colonne.
    nth1(EI, L, X),     % On récupère le bon élément.
    %write(X),nl,
    append(FINAL, [X], F), % Ajout à la diagonale
    %write(F),nl,
    decr(EI, NEI),
    incr(LI, NLI),
    %write("par : "), write(NEI), write(" , "), write(NLI), nl,
    diagonal_unit_dc(Board, NEI, NLI, F, R).

diagonal_unit_dc(_, 0, _, FINAL, R) :- append([], FINAL, R).
diagonal_unit_dc_b(Board, EI, LI, FINAL, R)  :-
    nth1(LI, Board,  L),% On récupère la colonne.
    nth1(EI, L, X),     % On récupère le bon élément.
    %write(X),nl,
    append(FINAL, [X], F), % Ajout à la diagonale
    %write(F),nl,
    decr(EI, NEI),
    incr(LI, NLI),
    %write("par : "), write(NEI), write(" , "), write(NLI), nl,
    diagonal_unit_dc_b(Board, NEI, NLI, F, R).
diagonal_unit_dc_b(_, _, 8, FINAL, R) :- append([], FINAL, R).


diagonal_hd(_, _, 7, FINAL, R) :- append([], FINAL, R).
diagonal_hd(_, 6, _, FINAL, R) :- append([], FINAL, R).
diagonal_hd(Board, EI, LI, FINAL, R) :-
    nth0(LI, Board,  L), nth0(EI, L, X),     % On récupère le bon élément.
    append(FINAL, [X], F), % Ajout à la diagonale
    incr(EI, NEI),incr(LI, NLI),
    diagonal_hd(Board, NEI, NLI, F, R).

diagonal_bg(_, _, -1, FINAL, R) :- append([], FINAL, R).
diagonal_bg(_, -1, _, FINAL, R) :- append([], FINAL, R).
diagonal_bg(Board, EI, LI, FINAL, R) :-
    nth0(LI, Board,  L), nth0(EI, L, X),     % On récupère le bon élément.
    append(FINAL, [X], F), % Ajout à la diagonale
    decr(EI, NEI),decr(LI, NLI),
    diagonal_bg(Board, NEI, NLI, F, R).

diagonal_hg(_, _, -1, FINAL, R) :- append([], FINAL, R).
diagonal_hg(_, 6, _, FINAL, R) :- append([], FINAL, R).
diagonal_hg(Board, EI, LI, FINAL, R) :-
    nth0(LI, Board,  L), nth0(EI, L, X),     % On récupère le bon élément.
    append(FINAL, [X], F), % Ajout à la diagonale
    incr(EI, NEI),decr(LI, NLI),
    diagonal_hg(Board, NEI, NLI, F, R).

diagonal_bd(_, _, 7, FINAL, R) :- append([], FINAL, R).
diagonal_bd(_, -1, _, FINAL, R) :- append([], FINAL, R).
diagonal_bd(Board, EI, LI, FINAL, R) :-
    nth0(LI, Board,  L), nth0(EI, L, X),     % On récupère le bon élément.
    append(FINAL, [X], F), % Ajout à la diagonale
    decr(EI, NEI),incr(LI, NLI),
    diagonal_bd(Board, NEI, NLI, F, R).


winnerDiagonal(Board, Player) :-
    diagonal_unit(Board,        1, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        2, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        3, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        4, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        5, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        6, 1, [], R), winnerColonne(R, Player);
    diagonal_unit(Board,        1, 2, [], R), winnerColonne(R, Player);
    diagonal_unit_b(Board,      1, 3, [], R), winnerColonne(R, Player);
    diagonal_unit_b(Board,      1, 4, [], R), winnerColonne(R, Player);
    diagonal_unit_b(Board,      1, 5, [], R), winnerColonne(R, Player);
    diagonal_unit_b(Board,      1, 6, [], R), winnerColonne(R, Player);
    diagonal_unit_b(Board,      1, 7, [], R), winnerColonne(R, Player);
    diagonal_unit_dc(Board,     6,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc(Board,     5,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc(Board,     4,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc(Board,     3,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc(Board,     2,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc(Board,     1,1,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,2,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,3,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,4,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,5,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,6,[], R),winnerColonne(R, Player);
    diagonal_unit_dc_b(Board,   6,7,[], R),winnerColonne(R, Player).


winnerColonne([Y|B], X) :-
    Y == X,
    B = [X,X,X|_];
    winnerColonne(B, X).

winnerLigne(Board, X, Num):-
    extract(Num, Board, L1),
    winnerColonne(L1, X).

isBoardFull(_, 7).
isBoardFull(Board, I) :-
    nth0(I, Board, Column),
    not(last('-', Column)),
    J is I+1, isBoardFull(Board, J).
isBoardFull(Board) :- isBoardFull(Board, 0).

isWinner(Board, Player) :-
    nth0(0, Board, C), winnerColonne(C, Player);
    nth0(1, Board, C), winnerColonne(C, Player);
    nth0(2, Board, C), winnerColonne(C, Player);
    nth0(3, Board, C), winnerColonne(C, Player);
    nth0(4, Board, C), winnerColonne(C, Player);
    nth0(5, Board, C), winnerColonne(C, Player);
    nth0(6, Board, C), winnerColonne(C, Player);
    winnerLigne(Board, Player, 0);
    winnerLigne(Board, Player, 1);
    winnerLigne(Board, Player, 2);
    winnerLigne(Board, Player, 3);
    winnerLigne(Board, Player, 4);
    winnerLigne(Board, Player, 5);
    winnerLigne(Board, Player, 6);
    winnerLigne(Board, Player, 7);
    winnerDiagonal(Board, Player).

win(Board) :-
    isWinner(Board, 'X'),
	write('X a gagné !'), nl, writeln('FIN'),
    replay;
	isWinner(Board, 'O'),
	write('O a gagné !'), nl, writeln('FIN'),
    replay;
    isBoardFull(Board),
    writeln('Égalité!'), nl, writeln('FIN'),
    replay.

replay :-
    writeln('Voulez-vous rejouer ? 1 (Oui) 2 (Non)'),
    read(P1),
    (
        P1==1 -> init
    ;   halt
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Affichage du plateau

display(_, 7, _).
display(N, I, Board) :-
    nth0(I, Board, Col), nth0(N, Col, Cell),
    write(Cell), write('  '), J is I+1, display(N, J, Board).

displayBoard(Board) :-
    nl, writeln(' 0  1  2  3  4  5  6'),
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
	writeln('Player 1, 1 (Human), 2 (Random IA), 3 (IA1), 4 (IA2)'),
	read(P1),
    writeln('Player 2, 1 (Human), 2 (Random IA), 3 (IA1), 4 (IA2)'),
	read(P2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% IA

% Jouer aléatoirement
randomIA(Board, Move, _) :-
    repeat,
	random_between(0, 6, Move),
    isValidMove(Move, Board),
    !.

% Prioriser les coups offrants
% le plus de possibilités d’alignement
ia1([], _, Move, _, Move, _, _).
ia1([T|Q], Board, Move, Player, NumeroColonneMax, CoutMax, NumeroColonneCourant) :-
    not(nth0(5, T, '-')), % on vérifie que la colonne n est pas pleine
    NouveauNumeroCourant is NumeroColonneCourant + 1,
    ia1(Q, Board, Move, Player, NumeroColonneMax, CoutMax, NouveauNumeroCourant); % si elle est pleine, on continue
    indexForColumn(T, 0, Index),
    heuristic1(NumeroColonneCourant, Index, Cout1),
    heuristiqueGagne(Board, NumeroColonneCourant, Player, Index, Cout2),
    Cout is Cout1 + Cout2,
    NouveauNumeroCourant is NumeroColonneCourant + 1,
    (
        Cout > CoutMax -> ia1(Q, Board, Move, Player, NumeroColonneCourant, Cout, NouveauNumeroCourant)
	;   ia1(Q, Board, Move, Player, NumeroColonneMax, CoutMax, NouveauNumeroCourant)
	).

% heuristique offrant le plus de possibilités d alignement
heuristic1(NumCol, NumLine, Cost):-
    HeuristicArray = [  [ 3 , 4 , 5 , 5 , 4 , 3 ],
                        [ 4 , 6 , 8 , 8 , 6 , 4 ],
                        [ 5 , 8 ,11 ,11 , 8 , 5 ],
                        [ 7 ,10 ,13 ,13 ,10 , 7 ],
                        [ 5 , 8 ,11 ,11 , 8 , 5 ],
                        [ 4 , 6 , 8 , 8 , 6 , 4 ],
                        [ 3 , 4 , 5 , 5 , 4 , 3 ] ],
    nth0(NumCol, HeuristicArray, Col),
    nth0(NumLine, Col, Cost).

% Heuristique pour savoir si on peut gagner (+1000)
heuristiqueGagne(Board, NumeroColonneCourant, Player, Index, Cout):-
    playMove(Board, NewBoard, NumeroColonneCourant, Player, Index),
    (
        isWinner(NewBoard, Player) -> Cout is 10000
    ;   changePlayer(Player, NextPlayer), playMove(Board, NewBoard2, NumeroColonneCourant, NextPlayer, Index), isWinner(NewBoard2, NextPlayer), Cout is 9000
    ;   Cout is 0
    ).

% Compter le nombre de pions alignés dans une colonne par rapport au dernier pion inséré
compterAvant(_, -1, _, Cout, Cout).
compterAvant(Col, Index, Player, Cout, CoutFin):-
    % write("Index : "), writeln(Index),
    nth0(Index, Col, Val),
    % write(Index), write(" : "), writeln(Val),
    ((Player == Val,
    NewCout is Cout + 1,
    NewIndex is Index-1,
    % write("Cout "), writeln(NewCout),
    compterAvant(Col, NewIndex, Player, NewCout, CoutFin)
    );
    (Player \= Val,
    compterAvant(Col, -1, Player, Cout, CoutFin)
    )).

% heuristique permettant donner un cout en fonction du nombre de pions alignés verticalement
heurCol(Col, Index, Player, Cout):-
    compterAvant(Col, Index, Player, 0, NbAligne),
    (
        (NbAligne == 1, Index > 2;
        NbAligne == 2, Index > 3;
        NbAligne == 3, Index > 4) 
        -> Cout is 0 ;
        Cout is NbAligne*NbAligne*NbAligne
    ).

% Prioriser les coups offrant
% le plus de possibilités d’alignement
% et réduisant ceux de l’adversaire
ia2([], _, Move, _, Move, _, _).
ia2([T|Q], Board, Move, Player, NumeroColonneMax, CoutMax, NumeroColonneCourant) :-
    %nth0(NumeroColonneCourant, Board, Col),
    not(nth0(5, T, '-')), % on vérifie que la colonne n est pas pleine
    NouveauNumeroCourant is NumeroColonneCourant + 1,
    ia2(Q, Board, Move, Player, NumeroColonneMax, CoutMax, NouveauNumeroCourant); % si elle est pleine, on continue
    writeln("IA2"),
    indexForColumn(T, 0, Index),
    heuristiqueVoisins(Board, NumeroColonneCourant, Player, Index, Cout1),
    changePlayer(Player, AutrePlayer),
    heuristiqueVoisins(Board, NumeroColonneCourant, AutrePlayer, Index, Cout5),
    writeln("\n FINISHING FOR IA2 \n"),
    heuristic1(NumeroColonneCourant, Index, Cout2),
    heuristiqueGagne(Board, NumeroColonneCourant, Player, Index, Cout3),
    heuristiqueAdverseGagne(Board, NumeroColonneCourant, Player, Index, Cout4),
    
    Cout is (Cout1 + Cout2 + Cout3 + Cout4 + Cout5),
    NouveauNumeroCourant is NumeroColonneCourant + 1,
    (
        Cout > CoutMax -> ia2(Q, Board, Move, Player, NumeroColonneCourant, Cout, NouveauNumeroCourant)
	;   ia2(Q, Board, Move, Player, NumeroColonneMax, CoutMax, NouveauNumeroCourant)
	).
% testHeuristiqueAdverse(Cout).
testHeuristiqueAdverse(Cout):-
  Array = [
      [ 'X',  'X',  '-',  '-',  '-', '-' ],
      [ 'X',  'X',  '-',  '-',  '-', '-' ],
      [ '-',  '-',  '-',  '-',  '-', '-' ],
      [ 'O',  'O',  '-',  '-',  '-', '-' ],
      [ '-',  '-',  '-',  '-',  '-', '-' ],
      [ '-',  '-',  '-',  '-',  '-', '-' ],
      [ '-',  '-',  '-',  '-',  '-', '-' ]
  ],
  heuristiqueAdverseGagne(Array, 2, 'O', 0, Cout).

% l adversaire gagne t il si je joue sur cette case
heuristiqueAdverseGagne(Board, NumeroColonneCourant, Player, Index, Cout):-
    playMove(Board, NewBoard, NumeroColonneCourant, Player, Index),
    changePlayer(Player, NextPlayer),
    Index1 is Index + 1,
    playMove(NewBoard, NewBoard2, NumeroColonneCourant, NextPlayer, Index1),
    isWinner(NewBoard2, NextPlayer) -> Cout is -8000 ; Cout is 0.

% heuristique de l IA2
heuristiqueVoisins(Board, NumeroColonneCourant, Player, Index, Cout):-
    write("############### COLONNE TESTEE : "), write(NumeroColonneCourant),
    writeln(" ###############"),

    playMove(Board, NewBoard, NumeroColonneCourant, Player, Index),

 	% Récupère la colonne
 	nth0(NumeroColonneCourant, NewBoard, C),
 	write("Colonne      : "),writeln(C),
    heurCol(C, Index, Player, CoutCol),

 	%Récupère la ligne
 	extract(Index, NewBoard, Line),
 	write("Ligne        : "), writeln(Line),
    heurLine(Line, Player, CoutLigne, 0),

    % Récupère la diagonale hd,
    diagonal_hd(NewBoard, Index, NumeroColonneCourant, [], R1),
    decr(Index, I2), decr(NumeroColonneCourant, N2),
    diagonal_bg(NewBoard, I2, N2, [], R2),
    append(R1,R2,DHD),
    write("Diagonale 1  : "),writeln(DHD),
    heurLine(DHD, Player, CoutDiag1, 0),

    diagonal_hg(NewBoard, Index, NumeroColonneCourant, [], R3),
    decr(Index, I4), incr(NumeroColonneCourant, N4),
    diagonal_bd(NewBoard, I4, N4, [], R4),
    append(R3,R4,DHGBD),
    write("Diagonale 2  : "), writeln(DHGBD),
    heurLine(DHGBD, Player, CoutDiag2, 0),

    %write("COUTLIGNE    : "),writeln(CoutLigne),
    %write("COUTCOLONNE  : "),writeln(CoutCol),
    %write("COUTDIAG1    : "),writeln(CoutDiag1),
    %write("COUTDIAG2    : "),writeln(CoutDiag2),

    %random(0,100, Cout),
    %Cout is CoutLigne + CoutCol,
    %write("COUT         : "),writeln(Cout).

    Cout is (CoutLigne + CoutCol + CoutDiag1 + CoutDiag2).

% Analyse d un tableau Line pour en sortir le cout total
heurLine(Line, _, TotalCost, TotalCost):-
    length(Line, Longueur),
    Longueur < 4.
heurLine(Line, Player, FinalCost, Cost):-
    take(4, Line, Quadruplet),
    Line=[_|Q],
    changePlayer(Player, NextPlayer),
    (
        changePlayer(Player, NextPlayer), member(NextPlayer, Quadruplet) -> heurLine(Q, Player, FinalCost, Cost)
	;   count(Player, Quadruplet, Occur), CostQuadruplet is Cost+Occur*Occur*Occur, heurLine(Q, Player, FinalCost, CostQuadruplet)
	).

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
	P1 == 1,
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(PossibleMove),
	(  not(isValidMove(PossibleMove, Board)) -> currentMove([P1|_], Player, Move, Board)
	;   Move is PossibleMove
	).

currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 1,
	writeln('Sur quelle colonne voulez-vous jouer ?'),
	read(PossibleMove),
	( not(isValidMove(PossibleMove, Board)) -> currentMove([_|P2], Player, Move, Board)
	; Move is PossibleMove
	).

currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 3,
	ia1(Board, Board, Move, Player, 0, 0, 0).

currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 3,
    ia1(Board, Board, Move, Player, 0, 0, 0).

currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 4,
	ia2(Board, Board, Move, Player, 0, 0, 0).

currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 4,
    ia2(Board, Board, Move, Player, 0, 0, 0).

currentMove([P1|_], Player, Move, Board) :-
	Player == 'X',
	P1 == 2,
	randomIA(Board, Move, Player).

currentMove([_|P2], Player, Move, Board) :-
	Player == 'O',
	P2 == 2,
    randomIA(Board, Move, Player).

% TBD
isValidMove(Move, Board) :-
	Move >= 0,
    Move < 7,
    indexForMove(Board, Move, Index),
    Index < 6,
    Index >= 0.

% returns the index of first empty space for a move
indexForMove(Board, Move, Index) :-
	nth0(Move, Board, Col),
    indexForColumn(Col, 0, Index).

indexForColumn([T|_], Index, Index) :- T = '-'.
indexForColumn([_|H], Index, NewIndex) :-
    Tmp is Index+1,
    indexForColumn(H, Tmp, NewIndex).

playMove(Board, NewBoard, Move, Player, Index) :-
	Index < 7,
    nth0(Move, Board, Col),
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
    departs(Board, NewBoard),
    humanOrAI(Player),        % Initialisation des joueurs
    play('X', NewBoard, Player). % Début du jeu

departs(Board, NewBoard) :-
     random(0,6, Coup),
     indexForMove(Board, Coup, Index),
     playMove(Board, IBoard, Coup, 'X', Index),
     random(0,6, C2),
     indexForMove(IBoard, C2, I2),
     playMove(IBoard, NewBoard, C2, 'O', I2),
     displayBoard(NewBoard),
     sleep(1).

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

extract(ColNumber, Matrix, Column) :-
    maplist(nth0(ColNumber), Matrix, Column).
    % write(Column).

% Incrementation of a variable
incr(X,X1) :- X1 is X+1.

% Decrementation of a variable
decr(X, X1) :- X1 is X-1.

% X est le dernier élément de la liste Y
% Sert notamment au prédicat isBoardFull
last(X,Y) :- append(_,[X],Y).

% renvoie le nombre d'occurrences d'un élément dans une liste
count(_, [], 0).
count(Elem, [Elem|Q], OccurFinal) :-
    !,
    count(Elem, Q, Occur),
    OccurFinal is Occur + 1.
count(Elem, [_|Q], Occur) :-
    count(Elem, Q, Occur).
