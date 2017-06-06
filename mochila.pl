% Por enunciado:
herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).
composicion(binaria(X,Y),P,5):- herramienta(X, PX), herramienta(Y,PY), X\= encendedor, P is 2*PX + PY.

% Ej1: composicion jerarquica
% composicion(+Composicion, ?Potencial, ?Costo)
composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), herramienta(Y, PY), P is PX * PY, C is 4. 
composicion(jerarquica(X,Y), P, C) :- herramienta(X, PX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CY + 1). 
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), herramienta(Y, PY), P is PX * PY, C is 2*(CX + 1).
composicion(jerarquica(X,Y), P, C) :- composicion(X, PX, CX), composicion(Y, PY, CY), P is PX * PY, C is 2*(CX+CY).

% Ej2:  configuracion
% configuracion(+Mochila, ?Configuracion, ?Potencial, ?Costo)
configuracion([X], X, P, C) :- herramienta(X, P), C is 1.
configuracion(M, Conf, P, C) :- esPermutacion(M, L), mochilaSegunConf(Conf, L), composicion(Conf, P, C). 

% mochilaSegunConf(?Conf, ?L)
% PRE: alguna de las dos variables debe estar instanciada
mochilaSegunConf(X, [X]) :- herramienta(X, _).
mochilaSegunConf(binaria(X,Y), L) :- herramienta(X,_), herramienta(Y,_), esPermutacion(L, [X,Y]).
mochilaSegunConf(jerarquica(X, Y), L) :- nonvar(L), particion(L,L0, L1), L0\= [], L1\=[], mochilaSegunConf(X, L0), mochilaSegunConf(Y, L1).
mochilaSegunConf(jerarquica(X, Y), L) :- ground(jerarquica(X, Y)), var(L), mochilaSegunConf(X, L1), mochilaSegunConf(Y, L2), append(L1, L2, L), !.

% particion(+L, -L1, -L2)
particion([],[],[]).
particion([X|XS],[X|L],R):-particion(XS,L,R).
particion([X|XS],L,[X|R]):-particion(XS,L,R).

% esPermutacion(?L1, ?L2)
esPermutacion([], []).
esPermutacion([X], [X]).
esPermutacion([T|H], X) :- ground([T|H]), var(X), H \= [], esPermutacion(H, H1), append(L1, L2, H1), append(L1, [T], X1), append(X1, L2, X).
esPermutacion(X, [T|H]) :- ground([T|H]), H \= [], esPermutacion(H, H1), append(L1, L2, H1), append(L1, [T], X1), append(X1, L2, X).

% Ej3: masPoderosa
% masPoderosa(+M1,+M2)
masPoderosa(M1, M2) :- configuracion(M1, _, P1, _), forall(configuracion(M2, _, P2, _), P1 > P2), !.

% Ej4: mejor
% mejor(+M1,+M2)
mejor(M1, M2) :- forall(configuracion(M2, _, P2, C2), (configuracion(M1, _, P1, C1), P1 >= P2, C1 < C2)).

% Ej5: usar
% usar(+M1,+Ps,?Cs,?M2)
usar(M1, Ps, Cs, M2) :- length(Ps, N), preservaElementos(Cs, N, M1, M2), todosMayores(Ps, Cs).

todosMayores(Ps,Cs) :- length(Ps,N), M is N-1, forall(entre0yN(I,M), mayorPotencial(Ps,Cs,I) ). 
entre0yN(I,N) :- between(0,N,I).
mayorPotencial(Ps,Cs,I) :- nth0(I,Ps,X), nth0(I,Cs,Y), composicion(Y,P,_), P >= X.

% preservaElementos(?Cs, +M1, ?M2)
preservaElementos(Cs, N, M1,M2) :- nonvar(Cs), length(Cs, N), mochilas(Cs, Ms), append(Ms, M3), esPermutacion(Maux, M1), append(M2, M3, Maux), !.
preservaElementos(Cs, N, M1,M2) :- var(Cs), var(M2), length(Cs, N), particion(M1, L, M2), particionN(L, PL, N), mochilas(Cs, PL).
preservaElementos(Cs, N, M1,M2) :- var(Cs), nonvar(M2), length(Cs, N), esPermutacion(M2, P2), particion(M1, L, P2), particionN(L, PL, N), mochilas(Cs, PL).

% mochilas(?Cs, ?Ms)
% PRE: Una de las dos viene definida (Cs o Ms)
mochilas(Cs, Ms) :- maplist(mochilaSegunConf,Cs,Ms).

% particionN(+L, -D, +N)
particionN(L, [L], 1).
particionN(L, D, N) :- N > 1, particion(L, L1, L2), M is N-1, particionN(L2, D2, M), append([L1], D2, D).

% Ej6: comprar
% comprar(+P,+C,?M)
comprar(P,C,M) :- between(1,C,I), generateMochila(M,I), maximoPotencial(M, Potencial), Potencial >= P.

% generateMochila(-Mochila, +CantidadDeHerramientasBasicas)
generateMochila([X],1) :- herramienta(X,_).
generateMochila([X|Xs],I) :- herramienta(X,_), I > 1, J is I-1, generateMochila(Xs,J).

% maximoPotencial(+Mochila, -PotencialMaximo)
maximoPotencial(M,P) :- configuracion(M,_,P,_), forall(configuracion(M,_,Paux,_), Paux =< P),!.