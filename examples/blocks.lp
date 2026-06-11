last:=5.
step(0..last-1).
time(0..last).
on(2,0):=table. on(3,0):=table. on(1,0):=3. on(4,0):=1.
goal(B):=B+1 :- B=1..3.
goal(4):=table.

block(B) :- on(B,0)=_.
location(B) :- block(B).
location(table).

clear(B,T) :- block(B), step(T), not on(_,T)=B.
clear(table,T) :- time(T).
1 {move_to(B,T+1):=L : clear(B,T), location(L), L!=B } 1 :- step(T).

on(B,T+1) := move_to(B,T+1) :- step(T).   % direct effect
:- step(T), not clear(move_to(_,T+1),T).  % executability
:- move_to(B,T+1)=table, on(B,T)=table.   % executability  
on(B,T+1):=on(B,T) :- step(T), not on(B,T+1)!=on(B,T).  % inertia
% with default notation (to be decided)
% on(B,T+1) ~= on(B,T) :- step(T).

% Last state: all blocks at goal position
:- on(B,last)!=goal(B).

#showf move_to/2.
%#showf on/2.