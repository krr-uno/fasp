next(X) := #some{Y : edge(X,Y)} :- node(X).
visited(Y) :- Y = #min{X:node(X)}.
visited(next(X)) :- visited(X).
:- node(X), not visited(X).