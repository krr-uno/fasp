next(X) := #some{Y : edge(X,Y)} :- node(X).
start := #min{X:node(X)}.
visited(start).
visited(next(X)) :- visited(X).
:- node(X), not visited(X).