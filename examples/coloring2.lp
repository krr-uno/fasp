{ color(X) := r;g;b } = 1 :- country(X).
:- neighbor(C,D), color(C)=color(D).

country(ch). country(fr). country(de).
neighbor(ch,fr).
neighbor(ch,de).
neighbor(fr,de).
