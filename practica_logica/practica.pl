%ap([],[],[]).
ap([],X,X).

ap([X|L],Y,[X|Z]) :- ap(L,Y,Z). 


last([X],X).
last([_|T],S) :- last(T,S).

l2(L,U) :- ap(L,U,[L|[U]]).

rev([],[]).
rev([X|T],S) :- rev(T,S1), append(S1,[X],S).


maxLista2([X],M):- M>=X.
maxLista2([X|T],M) :-  M>=X, member(M,T), maxLista2(T,M).

m2([X],X).

m2( [H|T], Z) :- m2(T,Z), Z=<H.

m2( [X|T], X) :- m2(T,Z), Z>=X.

pref(S,L) :- append(S,_,L).
suf(S,L) :- append(_,S,L).

sublista(S,L) :- pref(S1,L), suf(S,S1).

pert(X,L) :- sublista([X],L).

%apl([X|T],L) :- apl(T,S2), ap(X,S2,L).
%apl([[X]],[X]).
%apl([],[]).
%apl([[]],[]).

apl([],[]).
%Aplano la cabeza y la cola y concateno los resultados
apl([X|T],L) :- apl(X, SolHead), apl(T, SolTail), ap(SolHead,SolTail,L).

%La cabeza no puede ser una lista, ni vacia ni con contenido.
apl([X|Tail1],[X|Tail2]) :- X \= [], X \= [_|_], apl(Tail1, Tail2).

split(N,L1,L2,RES) :- ap(X,L4,L1).

desde(X,X).
desde(X,Y) :- var(Y), N is X + 1, desde(N,Y).
desde(X,Y) :- nonvar(Y), Y > X.

