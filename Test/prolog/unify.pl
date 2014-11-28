add1_1(X,Y):- Y = X + 1, 2 =:= 2.
add1_2(X,Y):- Y =:= X + 1.
add1_3(X,Y):- (Y + 2) is (X + 1).

test(X,Y) :- X = 2+1, Y = 5-3.
test2(X,Y) :- X = Y , Y = X.

test3(X,Y+1) :- X = Y, Y = X.

test4(X,X) :- X = 2.

test5(Y,X+1) :- Y = X+1.

addTwoThenMult3(X,Y):- Y is (X+2)*3, Y > 100.

