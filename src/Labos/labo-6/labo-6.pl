% ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
%          CARLOS MURCIA MORILLA


% EJERCICIO 1

% ?- por_encima_de(X,c) -> X = d
% ?- por_encima_de(c,X) -> X = b
% ?- por_arriba_ls(b, X) -> X = [c,d]
% ?- por_arriba_ls(X,Y) ->  X = d, Y = []
% ?- poner_encima(X,f) -> false
% ?- por_encima_de(X,Y), cima(Y) ->false
% ?- cima(Y), pila_izquierda(X,Y), cima(X) -> Y = g, X = d
% ?- pilas_contiguas(X,e), sobre(Y,X) ->  x = a, Y = b
% ?- por_arriba_ls(a,X), member(Y, X), por_encima_de(Z,Y) -> 
mas_por_encima_que(_, []).
mas_por_encima_que(X,Y) :- por_arriba_ls(X,Xs), por_arriba_ls(Y,Ys), mas_por_encima_que(Xs,Ys).
mas_por_encima_que([_|Xs], [_|Ys]) :- mas_por_encima_que(Xs, Ys).


% EJERICICO 2 
% Asumiendo que el enunciado te pide crear una nueva lista combinando las dos de forma ordenada, esta es la solución.
mezcla([],Y,Y).
mezcla(X,[],X).
mezcla([X|Xs],[Y|Ys],[X|Z]):- X @< Y, mezcla(Xs,[Y|Ys],Z).
mezcla([X|Xs],[Y|Ys],[Y|Z]):- mezcla([X|Xs],Ys,Z).

% EJERCICIO 3
my_reverse([],[]).
my_reverse(X, [X]).
my_reverse([X | Xs], Y):- my_reverse(Xs, Y).
es_simetrica(X):- X = my_reverse(X).
simetricas([Xs|Xss], Yss):- (es_simetrica(Xs)->simetricas(Xss, [Xs | Yss]); simetricas(Xs,Yss)).

% EJERCICIO 4 
numnodos(0, void).
numnodos(1 + Z1 + Z2, tree(_,I,D)):- numnodos(I,Z1), numnodos(D,Z2). 
