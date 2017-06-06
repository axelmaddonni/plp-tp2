:-[mochila].

:- begin_tests(lists).
:- use_module(library(lists)).

test("ej1-1") :-
        findall([P, C], composicion(jerarquica(jerarquica(binaria(rayo,volatilizador), rayo), rayo),P,C), L),
        L == [[6000, 26]].

test("ej2") :-
        findall([Conf, P, C], configuracion([rayo,volatilizador,rayo],Conf,P,C), L), member([jerarquica(volatilizador, binaria(rayo, rayo)), 1200, 12], L), member([jerarquica(volatilizador, jerarquica(rayo, rayo)), 4000, 10], L), !.

test("ej3-1") :-
        masPoderosa([volatilizador, volatilizador], [rayo, rayo]).

test("ej3-2") :-
        not(masPoderosa([rayo, rayo], [volatilizador, volatilizador])).

test("ej3-3") :-
        masPoderosa([encendedor, volatilizador], [encendedor, rayo]).

test("ej4-1") :-
        mejor([volatilizador,volatilizador],[rayo,rayo,rayo]).

test("ej5") :-
        findall(C, usar([rayo,rayo,volatilizador,rayo,encendedor],[30,80],C,[encendedor]), L), member([binaria(rayo, rayo), jerarquica(rayo, volatilizador)], L), member([binaria(rayo, rayo), binaria(volatilizador, rayo)], L), member([jerarquica(rayo, rayo), jerarquica(rayo, volatilizador)], L), !.

test("ej6-1") :-
        findall(M, comprar(100,2,M), L), member([rayo, rayo], L), !.

test("ej6-2") :-
        findall(M, comprar(100,2,M), L), member([rayo, volatilizador], L), !.

test("ej6-3") :-
        findall(M, comprar(100,2,M), L), member([volatilizador, volatilizador], L), !.

:- end_tests(lists).
