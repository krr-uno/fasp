country(france).
country(usa).
monarchy(spain).
citizen(felipe, spain).
citizen(juan, spain).

{king(C) := X : citizen(X,C)} = 1:- monarchy(C).

#showf king/1.