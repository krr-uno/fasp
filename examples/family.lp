father(cain)=adam.
father(abel)=adam.
mother(cain)=eve.
mother(abel)=eve.

% person(father(X)).
% person(mother(X)).
person(Y) :- father(_)=Y.
person(Y) :- mother(_)=Y.
person(X) :- father(X)=_.
person(X) :- mother(X)=_.
% male(father(X)).
% female(mother(X)).
male(Y) :- father(_)=Y.
female(Y) :- mother(_)=Y.

orphan(X) :- person(X), not father(X)=_, not mother(X)=_.

n_orphan = N :- N = #count{X : orphan(X)}.
