color(X) := #some{r;g;b} :- country(X).
:- neighbor(C,D), color(C)=color(D).

country(ch). country(fr). country(de).
neighbor(ch,fr).
neighbor(ch,de).
neighbor(fr,de).

#showf color/1.