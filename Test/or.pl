even(N):- 0 is mod(N,2).
big(N):- N > 100.

bigOrEven(N):- even(N) ; big(N).
bigAndEven(N):- big(N), even(N).