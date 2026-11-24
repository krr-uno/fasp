item(a).
candidate1(1,5).
candidate2(3..4).

{ value(X) := V : V=L..U, candidate1(L,U) } :- item(X).
{ value(X) := V : candidate2(V) } :- item(X).

% Rule in line 5
% { __f2_a_value(X) } :- item(X), V=L..U, candidate1(L,U).
% __f2_value(X) :- __f2_a_value(X).
% :- item(X), __f2_a_value(X), not &sum{ value(X) } >= L, not candidate1(L,U).
% :- item(X), __f2_a_value(X), not &sum{ value(X) } <= U, not candidate1(L,U).


% Rule in line 6
% { __f2_b_value(X) } :- item(X), candidate2(*).
% __f2_value(X) :- __f2_b_value(X).
% :- item(X), __f2_b_value(X), &sum{ value(X) } = V, not candidate2(V).