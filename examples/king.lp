country(france).
country(usa).
person(felipe).

{king(C,X) : person(X)}:- country(C).
{king(C) := X : person(X)}:- country(C).

:- king(C1,X), king(C2, X), C1 != C2.

% { f(X):=Y: p(X,Y) } = N.
