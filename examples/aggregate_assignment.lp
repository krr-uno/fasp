p(1..10).
q(5..15).
r(6..7).
intersection_count := #count{X : p(X), q(X), r(X)}.

#show.