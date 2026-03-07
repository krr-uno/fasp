#count { 0,__f_ASS(FUN,Y): __f_next(X,Y): edge(X,Y), __f_next(X,FUN) }  :- vertex(X); #count { Y: edge(X,Y) } >= 1.
__f_start(W) :- W = #min { X: vertex(X) }.
visited(FUN2) :- __f_start(FUN); __f_next(FUN,FUN2).
visited(FUN) :- visited(X); __f_next(X,FUN).
%  :- vertex(X); #false: visited(X).
% #showf next/1.
edge(a,b).
edge(a,d).
edge(b,c).
edge(b,e).
edge(c,a).
edge(d,f).
edge(e,d).
edge(f,c).
edge(f,e).
edge(X,Y) :- edge(Y,X).
vertex(X) :- edge(X,__A_0).
vertex(X) :- edge(__A_0,X).

#show visited/1.