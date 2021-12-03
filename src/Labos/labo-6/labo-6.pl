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
total_fichas(_, 0).
total_fichas(X, Acc):- (cima(X) = false -> total_fichas(X, Acc + 1); total_fichas(X, 0)).
mas_por_encima_que(X,Y) :- Accx > Accy, total_fichas(X, Accx), total_fichas(Y, Accy).


% EJERICICO 2 
mezcla([], [], []).
mezcla([X|Xs], [Y|Ys], [Z1,Z2|Zs]):- (X @<Y -> mezcla(Xs, Ys, [X | Z]); mezcla(Xs,Ys, [Y|Z])).

% EJERCICIO 3
my_reverse([], []).
my_reverse(X,[X]).
my_reverse([X | Xs], Y):-my_reverse(Xs, [X|Y]).
es_simetrica([]).
% es_simetrica([X]).
% es_simetrica([X | Xs]) :-
simetricas([Xs|Xss], Yss):- (es_simetrica(Xs)->simetricas(Xss, [Xs | Yss]); simetricas(Xs,Yss)).

% EJERCICIO 4 
numnodos(0, void).
numnodos(1 + Z1 + Z2, tree(_,I,D)):- numnodos(I,Z1), numnodos(D,Z2). 
