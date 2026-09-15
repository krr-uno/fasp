node(1).
color(red;blue).
1 { assign(N,C) : color(C) } 1 :- node(N).
#show assign/2.
