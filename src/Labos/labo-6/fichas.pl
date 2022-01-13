% Tenemos un conjunto de fichas apiladas en tres columnas (o pilas) sobre una mesa.
%  según el esquema siguiente:
%
%   d
%   c   g
%   b   f   i
%   a   e   h
%  -----------
% Esta información se representa mediante los siguientes predicados.
% sobre(X,Y) <-> la ficha X esta sobre la ficha Y.
% izquierda(X,Y) <-> la ficha X esta inmediatamente a la izquierda 
% de la ficha Y.
% cima(X) <-> la ficha X esta en la cima de una columna.

% Hechos:
cima(i).
cima(d).
cima(g).

sobre(d,c).
sobre(c,b).
sobre(b,a).
sobre(g,f).
sobre(f,e).
sobre(i,h).


izquierda(c,g).
izquierda(b,f).
izquierda(f,i).
izquierda(a,e).
izquierda(e,h).

% Se definen nuevos predicados para manejar esta información.


% por_encima_de(X,Y) <-> la ficha X esta en la misma pila que la ficha Y y más arriba.
% uso: por_encima_de(e/s,e/s).
por_encima_de(X,Y) :- sobre(X,Y).
por_encima_de(X,Y) :- sobre(X,Z), por_encima_de(Z,Y).

% por_encima_de_ERROR(X,Y).
%Llamadas recursivas infinitas cuando no hay más soluciones o el objetivo es falso.
% uso: por_encima_de_ERROR(e/s, e/s). 
por_encima_de_ERROR(X,Y) :- sobre(X,Y).
por_encima_de_ERROR(X,Y) :- por_encima_de_ERROR(X,Z), sobre(Z,Y).


% pila_izquierda(X,Y) <-> la ficha X está en la pila situada inmediatamente a la izquierda de 
%la pila en la que está la ficha Y
% uso: pila_izquierda(e/s,e/s)
pila_izquierda(X,Y) :- izquierda(X,Y).				% misma altura
pila_izquierda(X,Y) :- izquierda(Z,Y), por_encima_de(X,Z).	% X mas arriba que Y
pila_izquierda(X,Y) :- izquierda(X,Z), por_encima_de(Y,Z).	% X mas abajo que Y

 
% por_arriba(X,L) <-> L es la lista que contiene todas las fichas que están por encima de la ficha X.
% uso:  por_arriba(e/s,e/s).
por_arriba_ls(X,[]) :- cima(X).
por_arriba_ls(X,[Y|L]) :- sobre(Y,X), por_arriba_ls(Y,L).
 


% poner_encima(X,Y) <-> la ficha X se puede poner encima de la ficha Y  
% si ambas están en la cima de su pila, y en pilas contiguas.
% uso: poner_encima(e/s,e/s)
poner_encima(X,Y) :- cima(X), cima(Y), pilas_contiguas(X,Y).


% pilas_contiguas(X,Y) <-> la pila de la ficha X y la de la ficha Y están una al lado de la otra.
% uso: pilas_contiguas(e/s,e/s).
pilas_contiguas(X,Y) :- pila_izquierda(X,Y).
pilas_contiguas(X,Y) :- pila_izquierda(Y,X).

% ?- por_encima_de(X,c) -> X = d
% ?- por_encima_de(c,X) -> X = b
% ?- por_arriba_ls(b, X) -> X = [c,d]
% ?- por_arriba_ls(X,Y) ->  X = d, Y = []
% ?- poner_encima(X,f) -> false
% ?- por_encima_de(X,Y), cima(Y) ->false
% ?- cima(Y), pila_izquierda(X,Y), cima(X) -> Y = g, X = d
% ?- pilas_contiguas(X,e), sobre(Y,X) ->  x = a, Y = b
% ?- por_arriba_ls(a,X), member(Y, X), por_encima_de(Z,Y) ->
