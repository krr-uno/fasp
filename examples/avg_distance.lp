edge(1,2). edge(2,3). edge(1,3). edge(3,4).
edge(5,6). edge(6,7).
node(1..7).

dist(X,X):=0 :- node(X).
dist(X,Y):=M+1 :- dist(X,Z)=M, edge(Z,Y); #false: edge(W,Y), dist(X,W)=N, N<M.

% dist(X,Y):=M+1 :- M = #min{M: dist(X,Z)=M, edge(Z,Y)}.

% nested version
% dist(X,Y):=dist(X,Z)+1 :- edge(Z,Y); #false: edge(W,Y), dist(X,W)<dist(X,Z).