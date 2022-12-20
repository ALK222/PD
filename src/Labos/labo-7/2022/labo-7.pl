:- module(labo7_2022, [sumintersec/3, rama/2, ramas/2, genList/3, quitaMultiplos/3, eratostenes_aux/4, eratostenes/2]).

/** labo7
 * 
 * Laboratorio 7 de PD FDI UCM 22-23
 * 
 * @author Alejandro Barrachina Argudo
 * @author Carlos Murcia Morilla
 * 
 * @license GLP 3.0
 */


% =====================================================
% EJERCICIO 1
% =====================================================

%! sumintersec(+List, +List, -Num) is det.
%! sumintersec(-List, +List, -Num) is det.
%! sumintersec(+List, -List, ?Num) is det.
%
% Devuelve la suma de los elementos resultantes de la intersección de dos listas ordenadas
%
% Xs e Ys deben ser listas ordenadas
%
% Ejemplo:
% ==
% ?- sumintersec([1,2,3], [2,3], N).
% N = 5.
% ==
% ?- sumintersec([1,2,3], [2,3], 6).
% false.
%
%
% @arg Xs es la primera lista
% @arg Ys es la segunda lista
% @arg N es la suma de las intersecciones

sumintersec([], _, 0).
sumintersec(_, [], 0).
sumintersec([X|Xs], [Y|Ys], N ):- X == Y, sumintersec(Xs,Ys,N1), N is N1 + X.
sumintersec([X|Xs], [Y|Ys], N):- (X > Y -> sumintersec([X|Xs], Ys, N); sumintersec(Xs, [Y|Ys], N)).

% =====================================================
% EJERCICIO 2
% =====================================================

%! rama(+List, +arbol) is nondet.
%! rama(-List, +arbol) is nondet.
%! rama(?List, -arbol) is failure.
%
% Devuelve una rama completa de un árbol. 
%
% Consideramos que arbol tiene la estructura arbol(Elemento, arbol | void, arbol | void)
%
% Ejemplo:
% ==
% ?- rama(X, arbol(1, arbol(2, arbol(3,void,void), void), arbol(4, arbol(5,arbol(6, void, void),arbol(8, void, void)), arbol(7,void, void)))).
% X = [1, 2, 3].
%
%
% @arg Xs es la rama resultante
% @arg arbol es el arbol a analizar
% @arg X es el nodo actual del arbol
% @arg I es el hijo izquierdo del arbol
% @arg D es el hijo izquierdo del arbol
rama([X], arbol(X, void, void)):- !.
rama( [X|Xs], arbol(X, I, D) ) :- rama( Xs, I ) ; rama(Xs, D).

%! ramas(+List, +arbol) is nondet.
%! ramas(-List, +arbol) is nondet.
%! ramas(?List, -arbol) is failure.
%
% Devuelve todas las ramas completas de un árbol. 
%
% Consideramos que arbol tiene la estructura arbol(Elemento, arbol | void, arbol | void)
%
% Ejemplo
% ==
% ?- ramas(X, arbol(1, arbol(2, arbol(3,void,void), void), arbol(4, arbol(5,arbol(6, void, void),arbol(8, void, void)), arbol(7,void, void)))).
% X = [[1, 2, 3], [1, 4, 5, 6], [1, 4, 5, 8], [1, 4, 7]].
%
%
% @arg Rs es la lista de ramas
% @arg A es el arbol a analizar
%
% @see `rama/2`
ramas(Rs, A):- findall(As, rama(As, A), Rs).

% =====================================================
% EJERCICIO 3
% =====================================================

%! genList(+Num, +Num, -List) is det.
%! genList(-Num, ?Num, ?List) is failure.
%! genList(?Num, -Num, ?List) is failure.
%! genList(+Num, +Num, +List) is det.
%
% Genera una lista con números desde I hasta N.
%
% Ejemplo:
% ==
% ?- genList(2, 10, Ys).
% Ys = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10].
%
%
% @arg I número actual a meter en la lista
% @arg N número máximo de la lista
% @arg Ys lista de números
genList(N, N, [N]).
genList(I, N, [I|Ys]) :- I1 is I +1, genList(I1, N, Ys).

%! quitamultiplos(-Num, ?List, ?List) is failure.
%! quitaMultiplos(?Num, -List, ?List) is failure.
%! quitaMultiplos(+Num, +List, -List) is det.
%! quitaMultiplos(+Num, +List, +List) is det.
%
% Quita todos los múltiplos de un número dado de una lista dada, excepto el primero.
%
% Ejemplo:
% ==
% ?- quitaMultiplos(2, [2,3,4,5,6,7,8], Ys).
% Ys = [2,3,5,7].
%
%
% @arg N número a multiplicar
% @arg Xs lista de la que quitar los múltiplos
% @arg Ys lista resultante
quitaMultiplos(_, [],[]).
quitaMultiplos(N, [N|Xs], [N|Ys]):- quitaMultiplos(N, Xs, Ys).
quitaMultiplos(N, [X|Xs], Ys):- 0 is X mod N, quitaMultiplos(N, Xs, Ys).
quitaMultiplos(N, [X|Xs], [X| Ys]):- quitaMultiplos(N, Xs, Ys).

%! eratostenes_aux(-Num, ?Num, ?List, ?List) is failure.
%! eratostenes_aux(?Num, -Num, ?List, ?List) is failure.
%! eratostenes_aux(?Num, ?Num, -List, ?List) is failure.
%! eratostenes_aux(+Num, +Num, +List, -List) is det.
%! eratostenes_aux(+Num, +Num, +List, +List) is det.
%
% Hace la criba de Eratóstenes a un listado dado
%
% Ejemplo:
% ==
% ?- eratostenes_aux(2, 5, [2,3,4,5], Ps).
% Ps = [2,3,5].
%
%
% @arg I número actual
% @arg N número hasta el que llega la lista
% @arg L lista inicial
% @arg Ps lista cribada
eratostenes_aux(I, N, L, Ps):- N < I*I, quitaMultiplos(I, L, Ps).
eratostenes_aux(I, N, L, Ps):- N >= I * I, quitaMultiplos(I, L, Ps1), I1 is I + 1, eratostenes_aux(I1, N, Ps1, Ps).

%! eratostenes(-Num, ?List) is failure.
%! eratostenes(+Num, ?List) is det.
%
% Genera una lista desde 2 hasta N y luego le hace la criba de Eratóstenes.
%
% Ejemplo:
% ==
% eratostenes(5, Ps).
% Ps = [2,3,5].
%
%
% @arg N número al que llegar con la criba
% @arg Ps lista cribada
%
% @see `genList/3`
% @see `eratostenes_aux/4`

eratostenes(N, Ps):- genList(2, N, L), eratostenes_aux(2, N, L, Ps). 
