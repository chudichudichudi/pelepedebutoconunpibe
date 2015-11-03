listaNats(LInf,LSup,Nats) :- numlist(LInf,LSup,Nats).


% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.
% nPiezasDeCada(2,[1,2]) = [pieza(1,2), pieza(2,2)]

:-dynamic pieza/2. 

nPiezasDeCada(_,[],[]).
nPiezasDeCada(Cant,[X|T],P) :- nPiezasDeCada(Cant,T,Piezas2), asserta(pieza(X,Cant)), append([pieza(X,Cant)], Piezas2, P).

%%% Ejercicio 3Piezas

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 
cantidadDeApariciones(E,L,N) :- subtract(L,[E],L2), length(L,Tamanio1), length(L2,Tamanio2), N is Tamanio1 - Tamanio2.

resumenPiezas([], []).
resumenPiezas(L, [X|Tail]) :-  X = pieza(T,N), cantidadDeApariciones(T,L,N), N > 0, subtract(L,[T],L2) ,resumenPiezas(L2, Tail).

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

suma([], 0).
suma([X|T], Sum) :- X = pieza(Tam,Cant), suma(T,Sum2), Sum is Sum2 + Tam * Cant.



listaSuma([], 0).
listaSuma([X|T], S) :- between(1,S,N), X is N, S1 is S - X, listaSuma(T,S1).

generar(Total,Piezas,Sol) :- listaSuma(Soluciones,Total), resumenPiezas(Soluciones,Soluciones2), estanIncluidas(Soluciones2,Piezas),Sol = Soluciones2.

estanIncluidas([],_).
estanIncluidas([X|T],Piezas) :- esPiezaDe(X,Piezas), estanIncluidas(T,Piezas). 

esPiezaDe(_,[]) :- fail. 
esPiezaDe(X,[P|T]) :- X = pieza(T1,_), P = pieza(T2,_), T1 == T2; esPiezaDe(X,T).




%generar2(Total,Piezas,Sol):- length(Sol,N), N =< Total, 
%							member(X,Sol), member(Y,Piezas), 
%							between(0,Total, T1), between(0,Total, N1), X = pieza(T1,N1), 
%							Y = pieza(T2,_), T1 == T2,
%							suma(Sol, Sum), Sum == Total.


%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(_,[]).
cumpleLimite(Piezas,[pieza(T,N)| Tail]):- member(X,Piezas), X = pieza(T1,N1), T1 == T, N =< N1, cumpleLimite(Piezas, Tail).


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

construir1(Total,Piezas,Solucion):- generar(Total,Piezas,Solucion), cumpleLimite(Piezas, Solucion)	.


%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

% construir2(_,_,_):- fail.



%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(Total, Piezas, Soluciones, N):- construir1(Total, Piezas, Sol), member(Sol, Soluciones), length(Soluciones,N) .


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

%todosConstruir2(_, _, _, _):- fail.
