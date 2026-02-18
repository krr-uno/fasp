number(0..10).
fibo(0..1):=1.
fibo(X):=fibo(X-1) + fibo(X-2) :- number(X), X>1.

#show.