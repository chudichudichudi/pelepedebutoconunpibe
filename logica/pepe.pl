% listaNats(+LInf,+LSup,?Nats),
listaNats(LInf,LSup,Nats) :- LInf > LSup, Nats = [].
listaNats(LInf,LSup,Nats) :- LInf2 is LInf + 1, listaNats(LInf2, LSup, Nats2), append([LInf],Nats2, Nats).

listaNats2(LInf,LSup,Nats) :- numlist(LInf,LSup,Nats).


% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.

dynamic pieza. 

nPiezasDeCada(_,[],P) :- P = [].
nPiezasDeCada(Cant,[X|T],P) :- nPiezasDeCada(Cant,T,Piezas2), asserta(pieza(X,Cant)), append([pieza(X,Cant)], Piezas2, P).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 
%resumenPiezas([], _):- [].
resumenPiezas(SecPiezas, P):-  member()[].
