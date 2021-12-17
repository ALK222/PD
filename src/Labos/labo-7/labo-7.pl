% ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
%          CARLOS MURCIA MORILLA

% PARTE 1
elimina1([], X, []).
elimina1([X|R], Y, NR) :- Y == X, elimina1(R,Y,NR).
elimina1([X|R], Y, [X|NR]) :- Y\==X, elimina1(R,Y, NR).
% SOL: L =[b,c]

elimina2([], X, []).
elimina2([X|R], Y, NR) :- Y = X, elimina2(R,Y,NR).
elimina2([X|R], Y, [X|NR]) :- Y\=X, elimina2(R,Y, NR).
% SOL: L =[b,c]

elimina3([], X, []).
elimina3([X|R], X, NR) :- elimina3(R,X,NR).
elimina3([X|R], Y, [X|NR]) :- Y\==X, elimina3(R,Y, NR).
% SOL: L =[b,c]

% PARTE 2
% EJERCICIO 1

% arbol(ELEMENTO, IZQUIERDO, DERECHO)

maximo(void, 0).
maximo(arbol(E, Izq, Der), X) :- maximo(Izq, X1), % X1 es el maximo del hijo izquierdo
                                 maximo(Der,X2), % X2 es el maximo del hijo derecho
                                 (X1 > X2 -> N is X1; N is X2), % Se evalua que hijo tiene el mayor valor
                                 (E > N -> X is E; X is N). % Evaluamos si la raiz del nodo es mayor que el maximo de los hijos

% EJERCICIO 2
sublista( [], _ ).
sublista( [X|XS], [X|XSS] ) :- sublista( XS, XSS ).
sublista( [X|XS], [_|XSS] ) :- sublista( [X|XS], XSS ).

sublistas(LSX, Xs):- findall(X, sublista(X, Xs), LSX).


% EJERCICIO 3
aplana([],[]).
aplana([[H|T]|Xss], [H|X]) :- aplana([T|Xss], X).
aplana([H|Xss], [H|X]):- aplana(Xss, X).
% aplana(Xss, X) :-

% EJERCICO 4
hanoi(1,A,B,C,[A -> B]).
hanoi(N,A,B,C,L):- N>1, N1 is N-1, hanoi(N1,A,C,B,L1), hanoi(N1,C,B,A,L2), append(L1,[A -> B|L2],L).
