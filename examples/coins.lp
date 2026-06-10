coins(50):=3.
coins(20):=4.
coins(10):=2.
coins(5):=3.
fee:=235.
{ usedcoins(V) := 1..N} :- coins(V)=N.
:- #sum{usedcoins(V)*V}!=fee.
#maximize{usedcoins(V),V}.
