country(us).

king(spain) = felipe.
king(britain) = charles.
king(australia) = charles.

country(C) :- king(C) = _.

cosubject(P1,P2) :- person(P1), person(P2), P1 < P2, nationality(P1,C1), nationality(P2,C2), king(C1) = X, king(C2) = X, king(C1) = K1, king(C2) = K2, P1 != K1, P2 != K2.

nationality(jorge,spain).
nationality(josh,us).
nationality(zach,us).
nationality(victor,us).
nationality(victor,spain).
nationality(mary,britain).
nationality(john,australia).

person(P) :- nationality(P,_).
nationality(P,C) :- king(C) = P.

#show cosubject/2.