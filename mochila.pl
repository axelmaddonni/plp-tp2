herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).

composicion(binaria(X,Y),P,5):- herramienta(X, PX), herramienta(Y,PY), X\= encendedor, P is 2*PX + PY.

%% composicion(+Composicion, ?Potencial, ?Costo)

composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), herramienta(Y, PY), P is PX * PY, C is 4. 
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CX+CY). 
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), herramienta(Y, PY), P is PX * PY, C is 2*(CX + 1).
composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CY + 1). 

%% configuracion(+M, ?Conf, ?P, ?C)
configuracion([X], X, P, C) :- herramienta(X, P), C is 1.
configuracion(M, Conf, P, C) :- esPermutacion(M, L), mochilaSegunConf(Conf, L), composicion(Conf, P, C). 

%%mochilaSegunConf(?Conf, +L)
mochilaSegunConf(X, [X]) :- herramienta(X, PX).
mochilaSegunConf(binaria(X,Y), L) :- herramienta(X,PX), herramienta(Y,PY), esPermutacion(L, [X,Y]).
mochilaSegunConf(jerarquica(X, Y), L) :- L \= [], esPermutacion(L, L0), mochilaSegunConf(X, L1), append(L1, L2, L0), mochilaSegunConf(Y, L2).

%% esPermutacion(+L1, ?L2)
esPermutacion([], []).
esPermutacion([X], [X]).
esPermutacion([T|H], X) :- H \= [], esPermutacion(H, H1), append(L1, L2, H1), append(L1, [T], X1), append(X1, L2, X).

% masPoderosa(+M1,+M2)
masPoderosa(M1, M2) :- configuracion(M1, Conf1, P1, C1), forall(configuracion(M2, Conf2, P2, C2), P1 > P2), !.
% USAMOS CUT PARA QUE DEVUELVA UN SOLO VALOR ??

% mejor(+M1,+M2)
mejor(M1, M2) :- forall(configuracion(M2, Conf2, P2, C2), (configuracion(M, Conf, P, C), P >= P2, C < C2)), !.

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

comprar(P,C,M) :- between(1,C,I), generateLista(M,I), configuracion(M,Conf,Potencial,_), Potencial >= P.