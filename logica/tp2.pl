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
% ?- nPiezasDeCada(10,[1,2,3],X).
% X = [pieza(1, 10), pieza(2, 10), pieza(3, 10)] ;

nPiezasDeCada(_,[],[]).
nPiezasDeCada(Cant,[X|T],[pieza(X,Cant)|Piezas2]) :- nPiezasDeCada(Cant,T,Piezas2).

%%% Ejercicio 3

% resumenPiezas(+SecPiezas, -Piezas), que permite instanciar Piezas con la lista de
%  piezas incluidas en SecPiezas. 

% Cuenta la cantidad de apariciones de E en la lista L
% cantidadDeApariciones(+E,+L,-N)
% 
% Pruebas:
% resumenPiezas([1,1,2,2,3,3,4,4],S).

resumenPiezas([], []).
resumenPiezas([T|L], [pieza(T,N)|Tail]) :-subtract(L,[T],L2), 
										length(L2, Lengh2), length(L, Lengh1),
										N is Lengh1 - Lengh2,resumenPiezas(L2, Tail).

% ####################################
% Enfoque naïve
% ####################################

%%% Ejercicio 4

% generar(+Total,+Piezas,-Solución), donde Solución representa una lista de piezas
%  cuyos valores suman Total. Aquí no se pide controlar que la cantidad de cada pieza
%  esté acorde con la disponibilidad.

% Encuentra las listas que suman el Número instanciado
% listaSuma(+Lista, +N, -Sol).

listaSuma(_, 0, []).
listaSuma(Piezas, Total, [X|T]) :- member(pieza(X,C), Piezas), C > 0, append(I,[pieza(X,C)|D], Piezas),
									C1 is C - 1, append(I,[pieza(X,C1)|D], Piezas2),
									S1 is Total - X, S1 >= 0, listaSuma(Piezas2, S1, T).

% ?- listaSuma([1,2,3,4], 5, Sol).
% Sol = [1, 1, 1, 1, 1] ;
% Sol = [1, 1, 1, 2] ;
% Sol = [1, 1, 2, 1] ;
% Sol = [1, 1, 3] ;
% Sol = [1, 2, 1, 1] ;
% Sol = [1, 2, 2] ;
% Sol = [1, 3, 1] ;
% Sol = [1, 4] ;
% Sol = [2, 1, 1, 1] ;
% Sol = [2, 1, 2] ;
% Sol = [2, 2, 1] ;
% Sol = [2, 3] ;
% Sol = [3, 1, 1] ;
% Sol = [3, 2] ;
% Sol = [4, 1] ;
% false.

% generar(5, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol ).
generar(Total,Piezas,Sol) :-listaSuma(Piezas, Total, Sol), cumpleLimite(Piezas, Sol).

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
% true .
% 
% ?- cumpleLimite([pieza(2,3),pieza(4,3)],[2,2,2]).
% true .

% cumpleLimite(_,[]).
cumpleLimite(Piezas,Sol):-resumenPiezas(Sol, ResPiezas), cumple2(Piezas,ResPiezas).

cumple2(_,[]).
cumple2(Piezas, [pieza(T,X) | Tail ]) :- member(pieza(T,X1),Piezas),  X =< X1,
										cumple2(Piezas, Tail).

%%% Ejercicio 6

% construir1(+Total,+Piezas,-Solución), donde Solución representa una 
% lista de piezas cuyos valores suman Total y, además, las cantidades utilizadas
% de cada pieza no exceden los declarados en Piezas.

% Vamos viendo todas las listas que verifican sumar el Total y luego las
% filtramos por las que cumplen el límite.
% construir1(10, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol ).
construir1(Total,Piezas,Solucion):- generar(Total,Piezas,Solucion),
									cumpleLimite(Piezas, Solucion).

% ####################################
% Enfoque dinámico
% ####################################

%%% Ejercicio 7

% construir2(+Total,+Piezas,-Solución), cuyo comportamiento es id ́entico a
%  construir1/3 pero que utiliza definiciones dinámicas para persistir los
% cálculos auxiliares realizados y evitar repetirlos.  No se espera que las
% soluciones aparezcan en el mismo orden entre construir1/3 y construir2/3,
% pero sí, sean las mismas.

:-dynamic hecho/3.

% Encuentra las listas que suman el Número instanciado. Usamos asserta
%  para evitar repetir cálculos.
% listaSumaDinamica(-Lista,+Numero)

assertLD(Hecho):- \+( Hecho ),!, asserta(Hecho).
assertLD(_).

listaSuma2(P, S1, T) :- hecho(P, S1, T).
listaSuma2(Piezas, Total, [X|T]) :- \+(hecho(Piezas, Total,[X|T])),
									member(pieza(X,C), Piezas), C > 0, append(I,[pieza(X,C)|D], Piezas),
									C1 is C - 1, C1 >0, append(I,[pieza(X,C1)|D], Piezas2),
									S1 is Total - X, S1 >= 0, listaSuma2(Piezas2, S1, T),
									assertLD(hecho(Piezas, S1, T)).

listaSuma2(Piezas, Total, [X|T]) :- \+(hecho(Piezas, Total,[X|T])),
									member(pieza(X,C), Piezas), C > 0, append(I,[pieza(X,C)|D], Piezas),
									C1 is C - 1, C1 = 0, append(I,D, Piezas2),
									S1 is Total - X, S1 >= 0, listaSuma2(Piezas2, S1, T),
									assertLD(hecho(Piezas, S1, T)).


% generar2(5, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol ).
generar2(Total,Piezas,Sol) :- retractall(hecho(_,_,_)), asserta(hecho(_,0,[])),
							  	listaSuma2(Piezas,Total,Sol),
							  	cumpleLimite(Piezas, Sol).


% construir2(10, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol ).
construir2(Total,Piezas,Solucion):- generar2(Total,Piezas,Solucion).

% ####################################
% Comparación de resultados y tiempos
% ####################################

%%% Ejercicio 8

% todosConstruir1(+Total, +Piezas, -Soluciones, -N), donde Soluciones
% representa una lista con todas las
%  soluciones de longitud Total obtenidas con construir1/3, y N indica la
% cantidad de soluciones totales.

todosConstruir1(Total, Piezas, Soluciones, N):- findall( Sol, construir1(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .

%%% Ejercicio 9

% todosConstruir2(+Total, +Piezas, -Soluciones, -N), donde Soluciones representa una lista con todas 
%  las soluciones de longitud Total obtenidas con construir2/3, y N indica la cantidad de soluciones totales.

todosConstruir2(Total, Piezas, Soluciones, N):- findall( Sol, construir2(Total, Piezas, Sol), Soluciones), length(Soluciones,N) .


% ##########################################
% Resultados Compracion de Tiempos: 

% ?- time(todosConstruir1(12, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol, L )).
% 474,464 inferences, 0.047 CPU in 0.047 seconds (100% CPU, 10068025 Lips)
% Sol = [[1, 1, 1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1|...], [1, 1, 1, 1|...], [1, 1, 1|...], [1, 1|...], [1|...], [...|...]|...],
% L = 1489.

% ?- time(todosConstruir2(12, [pieza(1,10), pieza(2,10) , pieza(3,10) ,pieza(4,10)], Sol, L )).
% 460,850 inferences, 1.390 CPU in 1.390 seconds (100% CPU, 331435 Lips)
% Sol = [[1, 1, 1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1, 1|...], [1, 1, 1, 1, 1|...], [1, 1, 1, 1|...], [1, 1, 1|...], [1, 1|...], [1|...], [...|...]|...],
% L = 1489.



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

tienePatron(_,[]).
tienePatron(Patron,Lista) :- append(Patron, Resto, Lista),  tienePatron(Patron, Resto).

construirConPatron(Total, Piezas, Patron, Solucion) :- construir1(Total, Piezas, Solucion), tienePatron(Patron,Solucion).

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

