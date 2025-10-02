edge(1,2). edge(2,3). edge(1,3). edge(3,4).
edge(5,6). edge(6,7).
node(1..7).

dist(X,X):=0 :- node(X).

dist(X,Y):=M+1 :- edge(X,_), edge(_,Y), M = #min{N: dist(X,Z)=N, edge(Z,Y)}.
