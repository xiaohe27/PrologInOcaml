sis(joyce,niu).
sis(keke,joyce).
sis(joyce,ker).
sis(X,Y) :- sis(X,Z), sis(Z,Y).
?-sis(keke,X).

?-sis(keke,niu).

?-sis(X,Y).