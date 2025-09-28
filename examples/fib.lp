number(0..10).
fibo(0..1):=1.
% fibo(X) := fibo(X-1)+fibo(X-2) :- number(X),X>1.
fibo(X) := F1 :- number(X),X>1, fibo(X-1) = F1, fibo(X-2) = F2.