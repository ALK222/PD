% ALUMNOS: ALEJANDRO BARRACHINA ARGUDO
%          CARLOS MURCIA MORILLA
:-[fichas].%incluimos el fichero de fichas para mayor comodidad

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
mas_por_encima_que(X,Y) :- 
    por_arriba_ls(X,Xs), 
    por_arriba_ls(Y,Ys), 
    mas_por_encima_que(Xs,Ys).
% mas_por_encima_que([_|Xs], [_|Ys]) :- mas_por_encima_que(Xs, Ys).


% EJERICICO 2 
mezcla([],Y,Y).
mezcla(X,[],X).
mezcla([X|Xs],[Y|Ys],[X|Z]):- X @< Y, mezcla(Xs,[Y|Ys],Z).
mezcla([X|Xs],[Y|Ys],[Y|Z]):- mezcla([X|Xs],Ys,Z).

% EJERCICIO 3
% Implementación de reverse ya que no podemos utilizar el dado por Prolog
my_reverse(Xs,Ys) :- my_reverse(Xs,[],Ys).
my_reverse([],Y,Y).
my_reverse([X|Xs],Reverse,Aux) :- my_reverse(Xs,[X|Reverse],Aux).

simetricas([],[]).
simetricas([Xs|Xss], [Xs |Yss]):- my_reverse(Xs,Xs), simetricas(Xss, Yss).
simetricas([_|Xss], Yss):- simetricas(Xss,Yss).


% EJERCICIO 4 
numnodos(0, void).
numnodos(X, arbol(_,I,D)):- 
    numnodos(I,Z1), 
    numnodos(D,Z2),
    X is 1 + z1 + z2. 
