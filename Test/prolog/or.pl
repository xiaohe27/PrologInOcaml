even(N):- 0 is mod(N,2).
big(N):- N > 100.

bigOrEven(N):- even(N) ; big(N).
bigAndEven(N):- big(N), even(N).

test(M,N, X) :- M > 100, N is M+1 ; X is M + N.