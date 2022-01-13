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
% True if X has more elements above itsefl than Y.
%
% X and Y can be from different stacks.
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
% @arg X element to be compared
% @arg Y element used as comparator
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
% Takes two lists and makes a new list with all the elements of both lists in order.
%
% X and Y can be lists from different types.
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
% @arg X First list to be used
% @arg Y Second list to be used
% @arg Z Shorted list with elements from the two other lists
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
% Takes a list and creates its reverse.
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
% @arg X List to be used
% @arg Y Reversed list
my_reverse(Xs,Ys) :- my_reverse(Xs,[],Ys).

%! my_reverse(+List:list, -List:list, -List:list) is det.
%! my_reverse(+List:list, +List:list, -List:list) is det.
%! my_reverse(+List:list, +List:list, +List:list) is nondet.
%
% Auxiliary predicate for my_reverse/2
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
% @arg X List to be used
% @arg Y Auxiliary list
% @arg Z Reversed list
% @see my_reverse/2
my_reverse([],Y,Y).
my_reverse([X|Xs],Reverse,Aux) :- my_reverse(Xs,[X|Reverse],Aux).

%! simetricas(+List:list, -List:list) is det.
%! simetricas(+List:list, +List:list) is nondet.
%
% Takes a list of lists and returns a list with only the symmetric lists.
%
% All lists in X can be of different types
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
% @arg X List to be used
% @arg Yss List of symmetric lists
simetricas([],[]).
simetricas([Xs|Xss], [Xs |Yss]):- my_reverse(Xs,Xs), simetricas(Xss, Yss).
simetricas([_|Xss], Yss):- simetricas(Xss,Yss).


% =====================================================
% EJERCICIO 4
% =====================================================

%! numnodos(+Int:int, +Arbol:arbol) is det.
%
% Takes a tree a counts its nodes
%
% @arg X total of nodes
% @arg T tree to count
numnodos(0, void).
numnodos(X, arbol(_,I,D)):- 
    numnodos(I,Z1), 
    numnodos(D,Z2),
    X is 1 + Z1 + Z2. 
