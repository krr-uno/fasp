{ color(X,Y) : Y = r;g;b } = 1 :- country(X).
:- neighbor(C,D), color(C,Y), color(D,Y).

country(ch). country(fr). country(de).
neighbor(ch,fr).
neighbor(ch,de).
neighbor(fr,de).
