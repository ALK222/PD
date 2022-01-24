:- module(labo6, [mas_por_encima_que/2, mezcla/3, my_reverse/2, my_reverse/3, simetricas/2, numnodos/2]).
/** labo6
 * 
 * Laboratorio 6 de PD FDI UCM 21-22
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

%! mas_por_encima_que(+List:list, +List:list) is nondet.
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
mas_por_encima_que(_, []).
mas_por_encima_que(X,Y) :- 
    por_arriba_ls(X,Xs), 
    por_arriba_ls(Y,Ys), 
    mas_por_encima_que(Xs,Ys).


% =====================================================
% EJERCICIO 2
% =====================================================

%! mezcla(+List:list, +List:list, -List:list) is det.
%! mezcla(+List:list, +List:list, +List:list) is nondet.
%
% Coge dos lisras y hace una mezcla ordenada de ambas
%
% X e Y pueden ser de tipos distintos.
%
% Ejemplo:
% ==
% ?- mezcla([1,2],[2,3],Y).
% Y=[1,2,2,3].
% ?- mezcla([1,2],[2,3],[1,a]).
% false.
% ==
%
%
% @arg X Primera lista
% @arg Y Segunda lista
% @arg Z Lista final
mezcla([],Y,Y).
mezcla(X,[],X).
mezcla([X|Xs],[Y|Ys],[X|Z]):- X @< Y, mezcla(Xs,[Y|Ys],Z).
mezcla([X|Xs],[Y|Ys],[Y|Z]):- mezcla([X|Xs],Ys,Z).

% =====================================================
% EJERCICIO 3
% =====================================================

%! my_reverse(+List:list, -List:list) is det.
%! my_reverse(+List:list, +List:list) is nondet.
%
% Coge una lista y la invierte
%
% Ejemplo:
% ==
% ?- my_reverse([1,2],Y).
% Y=[2,1].
% ?- my_reverse([1,2],[1,2]).
% false.
% ==
%
%
% @arg X Lista dada
% @arg Y Lista invertida
my_reverse(Xs,Ys) :- my_reverse(Xs,[],Ys).

%! my_reverse(+List:list, -List:list, -List:list) is det.
%! my_reverse(+List:list, +List:list, -List:list) is det.
%! my_reverse(+List:list, +List:list, +List:list) is nondet.
%
% Predicado auxiliar para my_reverse/2
%
% Ejemplo:
% ==
% ?- my_reverse([1,2],[],Y).
% Y=[2,1].
% ?- my_reverse([1,2],Y,Z).
% Z=[2,1|Y].
% ?- my_reverse([1,2],[],[1,2]).
% false.
% ==
%
%
% @arg X Lista dada
% @arg Y Lista auxiliar
% @arg Z Lista invertida
% @see my_reverse/2
my_reverse([],Y,Y).
my_reverse([X|Xs],Reverse,Aux) :- my_reverse(Xs,[X|Reverse],Aux).

%! simetricas(+List:list, -List:list) is det.
%! simetricas(+List:list, +List:list) is nondet.
%
% Coge una lista de listas y devuelve una lista solo con aquellas que sean simetricas.
%
% Las listas dentro de X pueden ser de distintos tipoa
%
% Ejemplo:
% ==
% ?- simetricas([1,[1,2,2,1],[1,a]], Y).
% Y=[[1,2,2,1]].
% ?- simetricas([1,[1,a]],Z).
% Z = [].
% ?- simetricas([1,[1,2,2,1],[1,a]], [[1]]).
% false.
% ==
%
%
% @arg X Lista dada
% @arg Yss Lista de listas simétricas
simetricas([],[]).
simetricas([Xs|Xss], [Xs |Yss]):- my_reverse(Xs,Xs), simetricas(Xss, Yss).
simetricas([_|Xss], Yss):- simetricas(Xss,Yss).


% =====================================================
% EJERCICIO 4
% =====================================================

%! numnodos(+Int:int, +Arbol:arbol) is det.
%
% Coge un arbol y devuelve su número de nodos
%
% @arg X Numero de nodos
% @arg T Árbol a contar
numnodos(0, void).
numnodos(X, arbol(_,I,D)):- 
    numnodos(I,Z1), 
    numnodos(D,Z2),
    X is 1 + Z1 + Z2. 
