:-[mochila].

:- begin_tests(lists).
:- use_module(library(lists)).

test(composicion) :-
        findall([P, C], composicion(jerarquica(jerarquica(binaria(rayo,volatilizador), rayo), rayo),P,C), L),
        L == [[6000, 26]].

test(mejor) :-
        mejor([volatilizador,volatilizador],[rayo,rayo,rayo]).

:- end_tests(lists).
