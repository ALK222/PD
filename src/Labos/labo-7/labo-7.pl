:- module(labo7, [maximo/2, sublista/2, sublistas/2, aplana/2, aplana1/3, aplana1/2, hanoi/5]).
/** labo7
 * 
 * Laboratorio 7 de PD FDI UCM 21-22
 * 
 * @author Alejandro Barrachina Argudo
 * @author Carlos Murcia Morilla
 * 
 * @license GLP 3.0
 */
% PARTE 1
elimina1([], _, []).
elimina1([X|R], Y, NR) :- Y == X, elimina1(R,Y,NR).
elimina1([X|R], Y, [X|NR]) :- Y\==X, elimina1(R,Y, NR).
% 1 ?- elimina1([a,b,a,c],a,L).
% L = [b, c] .

% 2 ?- elimina1([a,b,a,c],X,L).
% L = [a, b, a, c].

elimina2([], _, []).
elimina2([X|R], Y, NR) :- Y = X, elimina2(R,Y,NR).
elimina2([X|R], Y, [X|NR]) :- Y\=X, elimina2(R,Y, NR).
% 3 ?- elimina2([a,b,a,c],a,L).
% L = [b, c] .

% 4 ?- elimina2([a,b,a,c],X,L).
% X = a,
% L = [b, c]

elimina3([], _, []).
elimina3([X|R], X, NR) :- elimina3(R,X,NR).
elimina3([X|R], Y, [X|NR]) :- Y\==X, elimina3(R,Y, NR).
% 5 ?- elimina3([a,b,a,c],a,L).
% L = [b, c] .

% 6 ?- elimina3([a,b,a,c],X,L).
% X = a,
% L = [b, c] .

% En el caso `elimina1` y en el `elimina2` cuando probamos la segunda consulta vemos como no salen los datos esperados porque
%  en el `elimina1` no hay igualdad sintactica nunca entre la variable Y y el termino X y en el `elimina2` solo unifica con el primer
% elemento de la lista y a partir de ahi no puede probar los demas terminos de la lista.

% PARTE 2

% =====================================================
% EJERCICIO 1
% =====================================================

%! maximo(-Arbol:arbol, -Int:int) is failure.
%! maximo(-Arbol:arbol, +Int:int) is det.
%! maximo(+Arbol:arbol, +Int:int) is det.
%
% Saca el elemento máximo encontrado en un arbol binario.
%
% @arg X es el elemento máximo
% @arg arbol es el árbol en el que buscamos el máximo
maximo(void, 0).
maximo(arbol(E, Izq, Der), X) :- maximo(Izq, X1), % X1 es el maximo del hijo izquierdo
                                 maximo(Der,X2), % X2 es el maximo del hijo derecho
                                 (X1 > X2 -> N is X1; N = X2), % Se evalua que hijo tiene el mayor valor
                                 (E > N -> X is E; X = N). % Evaluamos si la raiz del nodo es mayor que el maximo de los hijos

% =====================================================
% EJERCICIO 2
% =====================================================

%! sublista(-List:list, -List:list) is det.
%! sublista(-List:list, +List:list) is det.
%! sublista(+List:list, -List:list) is det.
%
% Coge una lista y genera una de sus sublistas posibles.
%
% Ejemplo:
% ==
% ?- sublista([1,2], X).
% X=[].
% ==
%
% @arg X elemento común en ambas listas
% @arg Xs Lista final
% @arg Xss Lista a dividir en sublistas
sublista( [], _ ).
sublista( [X|XS], [X|XSS] ) :- sublista( XS, XSS ).
sublista( [X|XS], [_|XSS] ) :- sublista( [X|XS], XSS ).

%! sublistas(-List:list, -List:list) is failure.
%! sublistas(-List:list, +List:list) is failure.
%! sublistas(+List:list, -List:list) is det.
%! sublistas(+List:list, +List:list) is nondet.
%
% Coge una lista y genera todas sus sublistas posibles.
%
% Ejemplo:
% ==
% ?- sublistas(Xss, [1,[a]]).
% Xss = [[], [1], [1, [a]], [[a]]].
% ==
%
% @arg LSX Sublistas totales
% @arg Xs Lista a dividir en sublistas
sublistas(LSX, Xs):- findall(X, sublista(X, Xs), LSX).

% solucion de susana

sublista1(Xs,[_|Ys]) :- sublista1(Xs,Ys).
sublista1(Xs,Ys) :- prefijo(Xs,Ys).

prefijo([],_).
prefijo([X|Xs],[X|Ys]) :- prefijo(Xs,Ys).

sublistas1(Xs,Xss) :- setof(S,sublista1(S,Xs),Xss).

% =====================================================
% EJERCICIO 3
% =====================================================

% Sin usar append

%! aplana1(+List:list, -List:list) is det.
%! aplana1(+List:list, +List:list) is nondet.
%
% Aplana una lista de listas en una lista simple.
% Utiliza aplana1/3 como función auxiliar.
%
% Ejemplo:
% ==
% ?- aplana1([1,[a],2,[1,2]], R).
% R = [1, a, 2, 1, 2].
% ==
%
% @arg L lista de listas
% @arg R lista aplanada
aplana1(L,R) :-
    aplana1(L,[],Flat),
    !,
    R=Flat.
%! aplana1(+List:list, -List:list, -List:list) is det.
%! aplana1(+List:list, +List:list, +List:list) is nondet.
%
% Aplana una lista de listas en una lista simple.
aplana1([],R,R)     :- !. 
aplana1([H|T],A1,R) :- !,
    aplana1(H,A2,R), % Aplanamos la primera parte de la lista
    aplana1(T,A1,A2). % Aplanamos la segunda parte de la lista
aplana1(E,T,[E|T]). % Juntamos elemento y resto en una sola lista

% Usando Append

%! aplana(+List:list, -List:list) is det.
%! aplana(+List:list, +List:list) is nondet.
%
% Aplana una lista de listas en una lista simple.
%
% Ejemplo:
% ==
% ?- aplana([1,[a],2,[1,2]], R).
% R = [1, a, 2, 1, 2].
% ==
%
% @arg H cabeza de la lista de listas
% @arg Xss lista de listas
% @arg Xs lista aplanada
aplana([],[]):-!.

aplana([H|Xss],Xs):-
    !,
    aplana(H, FlatH), % Aplanamos la primera parte de la lista
    aplana(Xss, FlatXss), % Aplanamos el resto de la lista
    append(FlatH, FlatXss, Xs). % Union de las listas aplanadas
aplana(L,[L]).

% solucion de susana
aplana2([],[]):-!.
aplana2([[]|L],LA):- !, aplana(L,LA).
aplana2([[X|Xs]|L], LA) :- !, append([X|Xs], L, L1), aplana2(L1,LA).
aplana2([X|L], [X|LA]):- atomic(X), aplana(L,LA).


% =====================================================
% EJERCICIO 4
% =====================================================

%! hanoi(-Int:int, -String:string, -String:string, -String:string, -List:list) is failure.
%! hanoi(+Int:int, +String:string, +String:string, +String:string, -List:list) is det.
%! hanoi(+Int:int, +String:string, +String:string, +String:string, +List:list) is nondet.
%
% Devuelve una lista de movimientos para resolver el juego de las torres de Hanoi.
%
% Ejemplo:
% ==
% ?- hanoi(2, A,B,C, L).
% L = [(A, C),  (A, B),  (C, B)] 
% ==
%
% @arg N número de piezas
% @arg A nombre de la torre inicial
% @arg B nombre de la torre intermedia
% @arg C nombre de la torre final
% @arg L lista de movimientos
hanoi(1,A,B,_,[(A,B)]).
hanoi(N,A,B,C,L):- 
    N1 is N-1,
    hanoi(N1,A,C,B,L1), % Movimientos de la inicial a la final
    hanoi(N1,C,B,A,L2), % Movimientos de la final a la inicial
    append(L1,[(A,B)|L2],L). % Union de todos los movimientos en L
