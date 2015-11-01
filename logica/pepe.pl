% listaNats(+LInf,+LSup,?Nats),
% listaNats(LInf,LSup,Nats) :- LInf > LSup, Nats = [].
% listaNats(LInf,LSup,Nats) :- LInf2 is LInf + 1, listaNats(LInf2, LSup, Nats2), append([LInf],Nats2, Nats).

listaNats(LInf,LSup,Nats) :- numlist(LInf,LSup,Nats).


% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.
% nPiezasDeCada(2,[1,2]) = [pieza(1,2), pieza(2,2)]

dynamic pieza. 

nPiezasDeCada(_,[],[]).
nPiezasDeCada(Cant,[X|T],P) :- nPiezasDeCada(Cant,T,Piezas2), asserta(pieza(X,Cant)), append([pieza(X,Cant)], Piezas2, P).

%%% Ejercicio 3Piezas

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 
cantidadDeApariciones(E,L,N) :- subtract(L,[E],L2), length(L,Tamanio1), length(L2,Tamanio2), N is Tamanio1 - Tamanio2.

resumenPiezas([], []).
resumenPiezas(L, [X|Tail]) :-  X = pieza(T,N), cantidadDeApariciones(T,L,N), subtract(L,[T],L2) ,resumenPiezas(L2, Tail).

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

% generar(Total,Piezas,Sol):- member(X,Sol), X = pieza(T,N), member(Y,Piezas), Y = pieza(T2,N2), T = T2, N =< N2, suma(SOL) = Total
