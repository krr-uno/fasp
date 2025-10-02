edge(1,2). edge(2,3). edge(1,3). edge(3,4). 
edge(5,6). edge(6,7).
node(1..7).

dist(X,X,0) :- node(X).
%dist(X,Y,1) :- edge(X,Y).
dist(X,Y,M+1) :- dist(X,Z,M), edge(Z,Y); #false: edge(W,Y), dist(X,W,N), N<M.

#show dist/3.
% A simple check: the distance is unique
:- dist(A,B,N), dist(A,B,M), N!=M.