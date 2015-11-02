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

suma([], 0).
suma([X|T], Sum) :- X = pieza(Tam,Cant), suma(T,Sum2), Sum is Sum2 + Tam * Cant.  

%generar(Total,Piezas,Sol):-  suma(Sol, Sum), Sum == Total, member(X,Sol), member(Y,Piezas), X = pieza(T1,_), Y = pieza(T2,_), T1 is T2.


%generar(0,_,[]).
%generar(Total,Piezas,Sol):- listaNats()


generar2(Total,Piezas,Sol):- length(Sol,N), N =< Total, 
							member(X,Sol), member(Y,Piezas), 
							between(0,Total, T1), between(0,Total, N1), X = pieza(T1,N1), 
							Y = pieza(T2,_), T1 == T2,
							suma(Sol, Sum), Sum == Total.


%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(_,[]).
cumpleLimite(Piezas,[pieza(T,N)| Tail]):- member(X,Piezas), X = pieza(T1,N1), T1 == T, N =< N1, cumpleLimite(Piezas, Tail).


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

construir1(Total,Piezas,Solucion):- generar2(Total,Piezas,Solucion), cumpleLimite(Piezas, Solucion)	.

