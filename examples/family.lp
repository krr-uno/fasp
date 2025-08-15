% orphan(X) :- person(X), not \forsome Y ( father(Y,X) ).

father(cain)=adam.
father(abel)=adam.
mother(cain)=eve.
mother(abel)=eve.

person(father(X)).
person(mother(X)).
% person(X):- \E father(X).
% person(X):- \E mother(X).
% male(father(X)).
% female(mother(X)).

% orphan(X) :- person(X), not \E father(X), not \E mother(X).
