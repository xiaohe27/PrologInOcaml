sis(joyce,niu).
sis(keke,joyce).
sis(X,Y) :- sis(X,Z), sis(Z,Y).
?-sis(keke,niu).

?-sis(X,Y).