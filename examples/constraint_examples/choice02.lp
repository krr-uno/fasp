item(a).
candidate1(1..2).
candidate2(3..4).

{ value(X) := V : candidate1(V) } :- item(X).
{ value(X) := V : candidate2(V) } = 1 :- item(X).

% Rule in line 5
% { __f2_a_value(X) } :- item(X), candidate1(*).
% __f2_value(X) :- __f2_a_value(X).
% :- item(X), __f2_a_value(X), &sum{ value(X) } = V, not candidate1(V).


% Rule in line 6
% _f2_b_value(X) :- item(X).
% :- item(X), not candidate2(*).
% __f2_value(X) :- __f2_b_value(X).
% :- item(X), __f2_b_value(X), &sum{ value(X) } = V, not candidate2(V).