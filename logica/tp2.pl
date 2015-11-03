%Grupo Chimicurry
% Integrantes: 
% Juan Carlos Giudici 827/06
% Tomas Freilij 629/06
% Santiago Iriarte 617/08

% ####################################
% Calentando motores
% ####################################

%%% Ejercicio 1
% Utilizamos la funcion ya definida numList
% listaNats(+LInf,+LSup,?Nats), que unifica la lista Nats con los naturales en el rango [LInf, LSup], o una lista vacía si LSup < LInf.

listaNats(LInf,LSup,Nats) :- numlist(LInf,LSup,Nats).

%?- listaNats(10,14,S).
%S = [10, 11, 12, 13, 14].

%%% Ejercicio 2

% nPiezasDeCada(+Cant, +Tamaños, -Piezas), que instancia a Piezas con una lista que contiene 
%  una cantidad Cant de cada tamaño en la lista Tamaños.
% 
:-dynamic pieza/2. 

nPiezasDeCada(_,[],[]).
nPiezasDeCada(Cant,[X|T],P) :- nPiezasDeCada(Cant,T,Piezas2), asserta(pieza(X,Cant)), append([pieza(X,Cant)], Piezas2, P).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 

% Cuenta la cantidad de apariciones de E en la lista L
% cantidadDeApariciones(+E,+L,-N)
%
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


% Calcula la suma de los elementos de una lista de piezas 
% suma(+Lista,-Sol)
% 
suma([], 0).
suma([X|T], Sum) :- X = pieza(Tam,Cant), suma(T,Sum2), Sum is Sum2 + Tam * Cant.


% Encuentra las listas que suman el Número instanciado
% listaSuma(-Lista,+Numero)
% 

listaSuma([], 0).
listaSuma([X|T], S) :- between(1,S,N), X is N, S1 is S - X, listaSuma(T,S1).


% Decide si las piezas de Lista está en la lista de piezas
% estanIncluidas(+Lista,+Piezas)
% 

estanIncluidas([],_).
estanIncluidas([X|T],Piezas) :- esPiezaDe(X,Piezas), estanIncluidas(T,Piezas). 


% Decide si la pieza está en Piezas
% esPiezaDe(+Pieza,+Piezas)
% 

esPiezaDe(_,[]) :- fail. 
esPiezaDe(X,[P|T]) :- P = pieza(T2,_), X == T2; esPiezaDe(X,T).




generar(Total,Piezas,Sol) :- listaSuma(Soluciones,Total),
							 estanIncluidas(Soluciones,Piezas),
							 Sol = Soluciones.

%%% Ejercicio 5 

% cumpleLímite(+Piezas,+Solución) será verdadero cuando la cantidad
% de piezas utilizadas en Solución 
%  no exceda las cantidades disponibles indicadas en Piezas
% Tests: 
% ?- cumpleLimite([pieza(2,3)],[2,2,2,2]).
% false.
% 
% ?- cumpleLimite([pieza(2,3)],[2,2,2]).
% true ;
% false.
% 
% ?- cumpleLimite([pieza(2,3),pieza(4,3)],[2,2,2,4]).
% true ;
% true .
% 
% ?- cumpleLimite([pieza(2,3),pieza(4,3)],[2,2,2]).
% true .

cumpleLimite(_,[]).
cumpleLimite(Piezas,Sol):-resumenPiezas(Sol, ResPiezas), member(X,Piezas),
						  member(Y,ResPiezas), X = pieza(T1,N1),
						  Y = pieza(T2,N2), T1 = T2, N2 =< N1.

%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una 
% lista de piezas cuyos valores suman Total y, además, las cantidades utilizadas
% de cada pieza no exceden los declarados en Piezas.

% Vamos viendo todas las listas que verifican sumar el Total y luego las
% filtramos por las que cumplen el límite.

construir1(Total,Piezas,Solucion):- generar(Total,Piezas,Solucion),
									cumpleLimite(Piezas, Solucion)	.


% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a
%  construir1/3 pero que utiliza definiciones dinámicas para persistir los
% cálculos auxiliares realizados y evitar repetirlos.  No se espera que las
% soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3,
% pero sí, sean las mismas.

:-dynamic listaSumaDinamica/2. 

% Encuentra las listas que suman el Número instanciado. Usamos asserta
%  para evitar repetir cálculos.
% listaSumaDinamica(-Lista,+Numero)
% 


listaSumaDinamica([],0).
listaSumaDinamica([X|T], S) :- between(1,S,N), X is N, S1 is S - X, 
							   listaSumaDinamica(T,S1),
							   asserta(listaSumaDinamica([X|T],S)).

generar2(Total,Piezas,Sol) :- retractall(lisDin(_,_)),
							  listaSumaDinamica(Soluciones,Total),
							  estanIncluidas(Soluciones,Piezas), 
							  Sol = Soluciones.

construir2(Total,Piezas,Solucion):- generar2(Total,Piezas,Solucion),
									cumpleLimite(Piezas, Solucion).

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones
% representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la
% cantidad de soluciones totales.

todosConstruir1(Total, Piezas, Soluciones, N):- setof( Sol, construir1(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .


%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Total, Piezas, Soluciones, N):- setof( Sol, construir2(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .

% ####################################
% Patrones
% ####################################

%%% Ejercicio 10

% construirConPatron(+Total, +Piezas, ?Patrón, -Solución) será verdadero 
% cuando Solución sea una solución factible en los términos definidos 
% anteriormente y, además, sus piezas respeten el patrón indicado en Patrón. Se
% sugiere definir un predicado tienePatrón(+Lista, ?Patrón) que decida si
% Lista presenta el Patrón especificado.

% Tests:
% tienePatron(+Patron, ?Lista)
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

% ?- tienePatron([A,B,C], Lista).
% Lista = [] ;
% Lista = [A, B, C] ;
% Lista = [A, B, C, A, B, C] ;
% Lista = [A, B, C, A, B, C, A, B, C] ;


% Rota una lista a la derecha
% rotatelist (+Lista,-Resultado)
rotatelist([H|T], R) :- append(T, [H], R).

tienePatron2(_,[]).
tienePatron2([HP|Patron],[HL|Lista]) :- rotatelist([HP|Patron], R), tienePatron2(R, Lista), HP = HL.

tienePatron(Patron,Lista) :- length(Patron, LP), length(Lista, LL),
							 Q is mod(LL, LP), Q == 0, tienePatron2(Patron, Lista).


construirConPatron(Total, Piezas, Patron, Solucion) :- todosConstruir1(Total, Piezas, Soluciones,_),
													   member(Sol,Soluciones), tienePatron(Patron,Sol),  Solucion = Sol.

% Tests: 
% ?- construirConPatron(5, [pieza(1,10)], [A], Sol).
% A = 1,
% Sol = [1, 1, 1, 1, 1] ;
%?- construirConPatron(6, [pieza(1,10), pieza(2,10)], [A,B], Sol).
%A = B, B = 1,
%Sol = [1, 1, 1, 1, 1, 1] ;
%A = 1,
%B = 2,
%Sol = [1, 2, 1, 2] ;
%A = 2,
%B = 1,
%Sol = [2, 1, 2, 1] ;

