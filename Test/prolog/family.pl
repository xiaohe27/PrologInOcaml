  male(james1).
  male(charles1).
  male(charles2).
  male(james2).
  male(george1).

  female(catherine).
  female(elizabeth).
  female(sophia).
  female(catherine2).


  parent(charles1, james1).
  parent(elizabeth, james1).
  parent(charles2, charles1).
  parent(catherine, charles1).
  parent(james2, charles1).
  parent(sophia, elizabeth).
  parent(james2, elizabeth).
  parent(george1, sophia).
  parent(catherine2, sophia).
  parent(catherine, james2).

  grandparent(X,Y) :- parent(X,Z), parent(Z,Y).

  grandma(X,Y) :- grandparent(X,Y), female(X).
	
  grandpa(X,Y) :- grandparent(X,Y), male(X).

?- parent(charles1, george1). 
?- parent(charles1,X). 
?- parent(X,charles1). 

?- grandma(X,elizabeth).