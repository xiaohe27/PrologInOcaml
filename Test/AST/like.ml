likes(mary,food).
likes(mary,wine).
likes(john,wine).
likes(john,mary).
?- likes(mary,food). 
%yes.
?- likes(john,wine). 
%yes.
?- likes(john,food).
%no.