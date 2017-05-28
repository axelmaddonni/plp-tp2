:-[mochila].

:- begin_tests(lists).
:- use_module(library(lists)).

test(jerarquica) :-
        findall([P, C], composicion(jerarquica(jerarquica(binaria(rayo,volatilizador), rayo), rayo),P,C), L),
        L == [[6000, 26]].
:- end_tests(lists).