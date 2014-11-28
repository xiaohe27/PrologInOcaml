l([H|[H2|T]]) :- H is 2, H2 is 3.

add(X,Y,Z, A) :- Z is X + Y, A is Z / 2.

test(X,Y,Z,Q) :- Y is X + 1, Z is Y * 2, Y is 2, Q is Y - 1.

test2(X) :- X + 1 is 2.
