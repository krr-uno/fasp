node(4).
dist(1,1,0).
dist(2,2,0).
dist(3,3,0).
dist(4,4,0).
dist(1,2,1) :- min(1,2,0).
dist(1,3,1) :- min(1,3,0).
dist(2,3,1) :- min(2,3,0).
dist(3,4,1) :- min(3,4,0).
min(1,2,0) :- #min { 0 } = 0.
min(1,3,0) :- #min { 0; 1: dist(1,2,1) } = 0.
min(1,4,#sup) :- #min { 1: dist(1,3,1) } = #sup.
min(1,4,1) :- #min { 1: dist(1,3,1) } = 1.
min(2,2,#sup) :- #min { } = #sup.
min(2,3,0) :- #min { 0 } = 0.
min(2,4,#sup) :- #min { 1: dist(2,3,1) } = #sup.
min(3,2,#sup) :- #min { } = #sup.
min(3,3,#sup) :- #min { } = #sup.
min(3,4,0) :- #min { 0 } = 0.
#show dist/3.
#show.