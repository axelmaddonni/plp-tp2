%Ej1 Composicion

herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).

%% composicion(+Composicion, ?Potencial, ?Costo)
composicion(binaria(X,Y),P,5):- herramienta(X, PX), herramienta(Y,PY), X\= encendedor, P is 2*PX + PY.
composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), herramienta(Y, PY), P is PX * PY, C is 4. 
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CX+CY). 
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), herramienta(Y, PY), P is PX * PY, C is 2*(CX + 1).
composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CY + 1). 

%Ej2 Configuracion
%% configuracion(+M, ?Conf, ?P, ?C)
configuracion([X], X, P, C) :- herramienta(X, P), C is 1.
configuracion(M, Conf, P, C) :- esPermutacion(M, L), mochilaSegunConf(Conf, L), composicion(Conf, P, C). 

%%mochilaSegunConf(?Conf, ?L)
%%PRE: alguna de las dos variables debe estar instanciada
mochilaSegunConf(X, [X]) :- herramienta(X, _).
mochilaSegunConf(binaria(X,Y), L) :- herramienta(X,_), herramienta(Y,_), esPermutacion(L, [X,Y]).
mochilaSegunConf(jerarquica(X, Y), L) :- nonvar(L), L \= [], esPermutacion(L, L0), mochilaSegunConf(X, L1), append(L1, L2, L0), mochilaSegunConf(Y, L2).
mochilaSegunConf(jerarquica(X, Y), L) :- ground(jerarquica(X, Y)), var(L), mochilaSegunConf(X, L1), mochilaSegunConf(Y, L2), append(L1, L2, L0), esPermutacion(L, L0).

%% esPermutacion(?L1, ?L2)
esPermutacion([], []).
esPermutacion([X], [X]).
esPermutacion([T|H], X) :- ground([T|H]), var(X), H \= [], esPermutacion(H, H1), append(L1, L2, H1), append(L1, [T], X1), append(X1, L2, X).
esPermutacion(X, [T|H]) :- ground([T|H]), H \= [], esPermutacion(H, H1), append(L1, L2, H1), append(L1, [T], X1), append(X1, L2, X).

%Ej3 masPoderosa
% masPoderosa(+M1,+M2)
masPoderosa(M1, M2) :- configuracion(M1, _, P1, _), forall(configuracion(M2, _, P2, _), P1 > P2), !.

%Ej4 mejor
% mejor(+M1,+M2)
mejor(M1, M2) :- forall(configuracion(M2, _, P2, C2), (configuracion(M1, _, P1, C1), P1 >= P2, C1 < C2)), !.

%Ej5 usar
% usar(+M1,+Ps,?Cs,?M2)
usar(M1, Ps, Cs, M2) :- length(Ps, N), length(Cs, N), todosMayores(Ps, Cs), preservaElementos(Cs, M1, M2).

todosMayores(Ps,Cs) :- length(Ps,N), M is N-1, forall(entre0yN(I,M), mayor(Ps,Cs,I) ). 
entre0yN(I,N) :- between(0,N,I).
mayor(Ps,Cs,I) :- nth0(I,Ps,X), nth0(I,Cs,Y), X > Y.

% preservaElementos(?Cs, +M1, ?M2)
preservaElementos(Cs,M1,M2) :- mochilas(Cs, Ms), append(Ms, M3), append(M2, M3, Maux), esPermutacion(Maux, M1).
mochilas(Cs, Ms) :- maplist(confAMochila,Cs,Ms).

%%confAMochila(+Conf, -M)
confAMochila(X, [X]) :- herramienta(X,_).
confAMochila(binaria(X,Y), M) :- herramienta(X,_), herramienta(Y,_), esPermutacion([X,Y], M).
confAMochila(jerarquica(X,Y), M) :- confAMochila(X, M1), confAMochila(Y,M2), append(M1, M2, Maux), esPermutacion(Maux, M).

% EJ6. Comprar
generateLista([X],1) :- herramienta(X,_).
generateLista([X|Xs],I) :- herramienta(X,_), I > 1, J is I-1, generateLista(Xs,J).
mejorConfiguracion(M,P) :- configuracion(M,_,P,_), forall(configuracion(M,_,Paux,_), Paux =< P),!.
comprar(P,C,M) :- between(1,C,I), generateLista(M,I), mejorConfiguracion(M, Potencial), Potencial >= P.
