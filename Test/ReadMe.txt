Notice that for the program below, std prolog cannot find the answer
sis(keke,niu).

sis(joyce,niu).
sis(keke,joyce).
sis(X,Y) :- sis(X,Z), sis(Z,Y).

?-sis(X,Y).


For help, use ?- help(Topic). or ?- apropos(Word).

1 ?- sis(X,Y).
X = joyce,
Y = niu ;
X = keke,
Y = joyce ;
ERROR: Out of local stack

