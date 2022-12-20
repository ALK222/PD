:- module(labo6_2022, [mas_por_encima_que2/2, intercala/3, contenida/2, sufijos/2, sufijo/2, numnodos3/2, numnodos2/2]).
/** labo6
 * 
 * Laboratorio 6 de PD FDI UCM 22-23
 * 
 * @author Alejandro Barrachina Argudo
 * @author Carlos Murcia Morilla
 * 
 * @license GLP 3.0
 */
:-[fichas].%incluimos el fichero de fichas para mayor comodidad

% =====================================================
% EJERCICIO 1
% =====================================================

%! mas_por_encima_que2(+List:list, +List:list) is nondet.
%
% True si el elemento X tiene más elementos por encima que el elemento Y.
%
% X e Y pueden ser de distintas pilas.
%
% Ejemplo:
% ==
% ?- mas_por_encima_que(a,b).
% false.
% ?- mas_por_encima_que(h,i).
% true.
% ==
%
%
% @arg X elemento a comparar
% @arg Y elemento comparador
mas_por_encima_que2(_, []).
mas_por_encima_que2(X,Y) :- 
    por_arriba_ls(X,Xs), 
    por_arriba_ls(Y,Ys), 
    mas_por_encima_que(Xs,Ys).


% =====================================================
% EJERCICIO 2
% =====================================================

%! intercala(+List:list, +List:list, -List:list) is det.
%! intercala(+List:list, +List:list, +List:list) is nondet.
%
% Coge dos listas y hace una mezcla de ambas
%
% X e Y pueden ser de tipos distintos.
%
% Ejemplo:
% ==
% ?- intercala([1,2],[2,3],Y).
% Y=[1,2,2,3].
% ?- intercala([1,2],[2,3],[1,a]).
% false.
% ==
%
%
% @arg Xs Primera lista
% @arg Ys Segunda lista
% @arg Z Lista final
intercala(_,[],[]).
intercala([],_,[]).
intercala([X|Xs], Ys, [X| Z]):- intercala2(Xs, Ys, Z).
intercala2(Xs,[Y|Ys],[Y| Z]):- intercala(Xs,Ys,Z).

% =====================================================
% EJERCICIO 3
% =====================================================

%! contenida(+List:list, -List:list) is det.
%! contenida(+List:list, +List:list) is nondet.
%
% Coge una lista y comprueba si está dentro de otra
%
% Ejemplo:
% ==
% ?- contenida([1,2],[1,2,3]).
% true.
% ?- contenida([1,2],Y).
% Y= [a,c,d|_].
% ==
%
%
% @arg Xs Lista contenida
% @arg Ys Lista total
contenida([], _).
contenida([X |Xs], [X|Ys]) :- contenida(Xs,[X |Ys]).
contenida([X | Xs], [_ | Ys]):- contenida([X | Xs], Ys).

% =====================================================
% EJERCICIO 4
% =====================================================

%! sufijos(+List:list, -List:list) is det.
%! sufijos(+List:list, +List:list) is nondet.
%
% Coge una lista y comprueba si es una lista de sufijos de otra
%
% Ejemplo:
% ==
% ?- sufijos([1,2,3], [[1], [1,2], [1,2,3]]).
% true.
% ==
% ?- sufijos(Xs, [[1], [1,2], [1,2,3]]).
% Xs = [1,2,3].
%
%
% @arg Xs Lista de sufijos
% @arg Yss Lista de listas
sufijos([], []).
sufijos([Z | Xs], [Ys | Yss]):-
    sufijo(Z1, Ys),
    Z is Z1,
    sufijos(Xs, Yss).

%! sufijo(+List:list, -List:list) is det.
%! sufijo(+List:list, +List:list) is nondet.
%
% Coge una lista y saca su sufijo
%
% Ejemplo:
% ==
% ?- sufijo(Z, [1,2,3]).
% Z = 3.
% ?- contenida(1,[1,2,3]).
% false.
% ==
%
%
% @arg Z sufijo
% @arg Ys Lista
sufijo(Z, [Z]).
sufijo(Z, [_ | Ys]):- sufijo(Z, Ys).

% =====================================================
% EJERCICIO 5
% =====================================================

%! numnodos3(+Int:int, +Arbol:arbol) is det.
%
% Coge un arbol y devuelve su número de nodos
%
% @arg X Numero de nodos
% @arg T Árbol a contar
numnodos3(0, void).
numnodos3(X, arbol(_,I,D)):- 
    numnodos(I,Z1), 
    numnodos(D,Z2),
    X is 1 + Z1 + Z2. 


%! numnodos2(+Int:int, +Arbol:arbol) is det.
%
% Coge un arbol y devuelve su número de nodos
%
% @arg X Numero de nodos
% @arg T Árbol a contar
numnodos2(0, void).
numnodos2(s(X), arbol(_,I,D)):-
    numnodos2(I, Z1),
    numnodos2(D, Z2),
    X is Z1 + Z2.
