%ejercicio 4

concatenar([], X, X).
concatenar([X|L1], L2, [X|L3]) :- concatenar(L1, L2, L3). 

%ejercicio 5

last([X],X).
last([_|T],Z) :- last(T,Z) .

reverse([],[]).
reverse([X|L1],L2):- reverse(L1,L3), concatenar(L3,[X],L2).

maxlista([X], X).
maxlista([X,Y|L], M):- X >= Y, maxlista([X|L],M).
maxlista([X,Y|L], M):- X =< Y, maxlista([Y|L],M).

prefijo(P, L):- concatenar(P,_,L).

sufijo(S, L):- concatenar(_,S,L).

sublista(S,L):- prefijo(A,L), sufijo(S,A).

pertenece(X, [X|_]).
pertenece(X, [_|L]) :- pertenece(X,L).

%ejercicio 6

aplanar([],[]).
aplanar([X|T],YS):- aplanar(X,XS), aplanar(T,T1), concatenar(XS,T1,YS).
aplanar([X|Tail1],[X|Tail2]) :- X \= [], X \= [_|_], aplanar(Tail1, Tail2).

%ejercicio 7

palindromo(L,L1):- reverse(L,RL), concatenar(L,RL,L1).

doble([X], [X,X]).
doble([X|T],[X,X|T1]):- doble(T,T1).

iesimo(0,[X|_],X).
iesimo(I,[_|T],X):- N is I - 1, iesimo(N,T,X) .

%ejercicio 8

desde(X,X).
desde(X,Y) :- nonvar(Y), X >=Y.
desde(X,Y) :- var(Y), N is X+1, desde(N,Y).

%ejercicio 9

quitarRepetidos([X|T1], L) :- member(X,T1), quitarRepetidos(T1, L).
quitarRepetidos([X|T1], L) :- not(member(X,T1)), quitarRepetidos(T1, [X|L]).
interseccion([], X, X).
interseccion([X|T1], L, T3) :- \+(member(X,L)),
									interseccion(T1, [X|L], T3).
interseccion([X|T1], L, T3) :- member(X,L),
									interseccion(T1, T2, [X|T3]).

%ejercicio 10
%ejercicio 11
%ejercicio 12
%ejercicio 13
%ejercicio 14
%ejercicio 15
%ejercicio 16
%ejercicio 17


