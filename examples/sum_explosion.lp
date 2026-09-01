#const n = 10000.
#const forbidden = 537.

1 { a(X) : X = 1..n } 1.
1 { b(Y) : Y = 1..n } 1.

:- a(X), b(Y), X + Y == forbidden.

% % valid
% 1 { a(X) : X = 1..10000 } 1.
% 1 { b(Y) : Y = 1..10000 } 1.

:- a(X), b(Y), X + Y == 537.
#show a/1.
#show b/1.