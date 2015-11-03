% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1

% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

listaNats(LInf,LSup,Nats) :- numlist(LInf,LSup,Nats).

%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.

:-dynamic pieza/2. 

nPiezasDeCada(_,[],[]).
nPiezasDeCada(Cant,[X|T],P) :- nPiezasDeCada(Cant,T,Piezas2), asserta(pieza(X,Cant)), append([pieza(X,Cant)], Piezas2, P).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 

cantidadDeApariciones(E,L,N) :- subtract(L,[E],L2), length(L,Tamanio1), length(L2,Tamanio2), N is Tamanio1 - Tamanio2.

resumenPiezas([], []).
resumenPiezas(L, [X|Tail]) :-  X = pieza(T,N), cantidadDeApariciones(T,L,N), N > 0, subtract(L,[T],L2) ,resumenPiezas(L2, Tail).


% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.
suma([], 0).
suma([X|T], Sum) :- X = pieza(Tam,Cant), suma(T,Sum2), Sum is Sum2 + Tam * Cant.

listaSuma([], 0).
listaSuma([X|T], S) :- between(1,S,N), X is N, S1 is S - X, listaSuma(T,S1).

estanIncluidas([],_).
estanIncluidas([X|T],Piezas) :- esPiezaDe(X,Piezas), estanIncluidas(T,Piezas). 

esPiezaDe(_,[]) :- fail. 
esPiezaDe(X,[P|T]) :- X = pieza(T1,_), P = pieza(T2,_), T1 == T2; esPiezaDe(X,T).

generar(Total,Piezas,Sol) :- listaSuma(Soluciones,Total), resumenPiezas(Soluciones,Soluciones2), estanIncluidas(Soluciones2,Piezas),Sol = Soluciones2.

%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas

cumpleLimite(_,[]).
cumpleLimite(Piezas,[pieza(T,N)| Tail]):- member(X,Piezas), X = pieza(T1,N1), T1 == T, N =< N1, cumpleLimite(Piezas, Tail).


%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas cuyos valores 
%  suman Total y, además, las cantidades utilizadas de cada pieza no exceden los declarados en Piezas.

construir1(Total,Piezas,Solucion):- generar(Total,Piezas,Solucion), cumpleLimite(Piezas, Solucion)	.


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a construir1/3 pero que utiliza 
%  definiciones dinámicas para persistir los cálculos auxiliares realizados y evitar repetirlos. 
%  No se espera que las soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3, pero sí, sean las mismas.

%construir2(_,_,_):- fail.

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la cantidad de soluciones totales.

todosConstruir1(Total, Piezas, Soluciones, N):- findall( Sol, construir1(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Total, Piezas, Soluciones, N):- findall( Sol, construir2(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .

% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero cuando Solución sea una solución factible 
%  en los términos definidos anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. 
%  Se sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si Lista presenta el Patrón especificado.

%construirConPatron(_, _, _, _):- fail.
%tienePatron(+Patron, ?Lista)
% ?- tienePatron([A, A], [1,1,1,1]).
% A = 1.
% ?- tienePatron([B, B], [1,1,1]).
% false.
% ?- tienePatron([A, B], [2,1,2,1]).
% A = 2, B = 1.
% ?- tienePatron([A, B], [2,1,4,5]).
% false.
% ?- tienePatron([A, 1, B, 2, C], [2,1,3,2,7]).
% A = 2, B = 3, C = 7.

rotatelist([H|T], R) :- append(T, [H], R).

tienePatron2(_,[]).
tienePatron2([HP|Patron],[HL|Lista]) :- rotatelist([HP|Patron], R), tienePatron2(R, Lista), HP = HL.

tienePatron(Patron,Lista) :- ground(Lista), length(Patron, LP),
							 length(Lista, LL), Q is mod(LL, LP), Q == 0, tienePatron2(Patron, Lista).
						 

construirConPatron(Total, Piezas, Patron, Solucion) :- todosConstruir1(Total, Piezas, Soluciones,N),
													   member(Soluciones, Sol), tienePatron(Patron,Sol).