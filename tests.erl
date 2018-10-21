-module(tests).
-export([start/0]).

start() ->
    lists:map(fun interp:runStr/1,examples()).

examples() ->
    %[ex1(), ex2(), ex3(), ex4(), ex5(), ex6(), ex7(), ex8(), ex9()].
    [my7(), my8()].

ex1() ->
    "let x=1 in let x=3 in +(x,7)".

ex2() ->
    "+(2,3)".

ex3() ->
    "proc (x) +(x,3)".

ex4() ->
    "let y=3 in proc (x) +(x,y)".

ex5() ->
    "let y=3 in +(2,y)".

ex6() ->
    "let y=proc(x) +(x,1) in y(5)".

ex7() ->
    "let x=1 in let y=proc(z) +(z,x) in y(6)".

ex8() ->
    "zero?(7)".

ex9() ->
    "let x=1 in let f=proc(y) +(y,x) in let x=2 in f(3)".

my1() ->
    "3".

my2() ->
    "+(3,2)".

my3() ->
    "-(3,2)".

my4() ->
    "zero?(3)".

my5() ->
    "let x=0 in zero?(x)".

my6() ->
    "let x=5 in -(x,2)".

my7() ->
    "if zero?(0) then 1 else 2".

my8() ->
    "if zero?(1) then 1 else +(3,1)".



