% AVL-Tree
tree(K, V, tree(K, V, empty, empty, 1)).
tree(K, V, L, R, tree(K, V, L, R, H)) :- height(L, HL), height(R, HR), max(HL, HR, HM), H is HM + 1.

map_build(ListMap, TreeMap) :- map_build(ListMap, empty, TreeMap).
map_build([], Tree, Tree).
map_build([(K, V) | T], Tree, TreeMap) :- insert(Tree, K, V, TreeMap0), map_build(T, TreeMap0, TreeMap).

map_get(TreeMap, Key, Value) :- find(TreeMap, Key, Value).
map_put(TreeMap, Key, Value, Result) :- insert(TreeMap, Key, Value, Result).
map_remove(TreeMap, Key, Result) :- remove(TreeMap, Key, Result).
map_minKey(Map, Key) :- left(Map, empty), !, key(Map, Key).
map_minKey(Map, Key) :- left(Map, L), map_minKey(L, Key).
map_maxKey(Map, Key) :- right(Map, empty), !, key(Map, Key).
map_maxKey(Map, Key) :- right(Map, R), map_maxKey(R, Key).

max(A, B, B) :- A < B, !.
max(A, B, A).

left(empty, empty).
left(tree(_, _, L, _, _), L).
set_left(tree(K, V, _, R, H), L, Tree) :- tree(K, V, L, R, Tree).

right(empty, empty).
right(tree(_, _, _, R, _), R).
set_right(tree(K, V, L, _, H), R, Tree) :- tree(K, V, L, R, Tree).

key(tree(K, _, _, _, _), K).
value(tree(_, V, _, _, _), V).
set_value(tree(K, _, L, R, H), V, Tree) :- tree(K, V, L, R, Tree).

height(empty, 0).
height(tree(_, _, _, _, H), H).

find(tree(K, V, _, _, _), K, V).
find(tree(X, _, L, _, _), K, V) :- K < X, !, find(L, K, V).
find(tree(X, _, _, R, _), K, V) :- K > X, !, find(R, K, V).

insert(empty, K, V, T) :- tree(K, V, T).
insert(T, K, V, T2) :- key(T, X), K < X, !, left(T, L), insert(L, K, V, L1), set_left(T, L1, T1), balance(T1, T2).
insert(T, K, V, T2) :- key(T, X), K > X, !, right(T, R), insert(R, K, V, R1), set_right(T, R1, T1), balance(T1, T2).
insert(T, K, V, T2) :- key(T, X), K = X, !, set_value(T, V, T2).

find_min(T, T) :- left(T, empty).
find_min(T, M) :- left(T, L), find_min(L, M).

remove(T, K, empty) :- key(T, K), left(T, empty), right(T, empty), !.
remove(T, K, T) :- \+ key(T, K), left(T, empty), right(T, empty), !.
remove(T, K, L) :- key(T, K), right(T, empty), !, left(T, L).
remove(T, K, R) :- key(T, K), left(T, empty), !, right(T, R).
remove(T, K, T2) :- key(T, K), left(T, L), right(T, R), find_min(R, M), key(M, KM), value(M, VM), remove(R, KM, R1), tree(KM, VM, L, R1, T1), balance(T1, T2), !.
remove(T, K, T2) :- key(T, X), K < X, !, left(T, L), remove(L, K, L1), set_left(T, L1, T1), balance(T1, T2).
remove(T, K, T2) :- key(T, X), K > X, !, right(T, R), remove(R, K, R1), set_right(T, R1, T1), balance(T1, T2).

rotate_left(A, B1) :- right(A, B), left(B, BL), set_right(A, BL, A1), set_left(B, A1, B1).
rotate_right(A, B1) :- left(A, B), right(B, BR), set_left(A, BR, A1), set_right(B, A1, B1).
big_rotate_left(A, A2) :- right(A, AR), rotate_right(AR, AR1), set_right(A, AR1, A1), rotate_left(A1, A2).
big_rotate_right(A, A2) :- left(A, AL), rotate_left(AL, AL1), set_left(A, AL1, A1), rotate_right(A1, A2).

balance(A, A) :- left(A, L), right(A, R), height(L, HL), height(R, HR), D is HL - HR, (-1 is D; 0 is D; 1 is D).
% rotate left
balance(A, A1) :- left(A, L), right(A, B), left(B, C), right(B, R),
									 height(L, HL), height(C, HC), height(R, HR), height(B, HB),
									 2 is HB - HL, HC =< HR, !, rotate_left(A, A1).
% rotate right
balance(A, A1) :- left(A, B), right(A, R), left(B, L), right(B, C),
									 height(L, HL), height(C, HC), height(R, HR), height(B, HB),
									 2 is HB - HR, HC =< HL, !, rotate_right(A, A1).
% big rotate left
balance(A, A1) :- left(A, L), right(A, B), left(B, C), right(B, R),
									 height(L, HL), height(C, HC), height(R, HR), height(B, HB),
									 2 is HB - HL, HC > HR, !, big_rotate_left(A, A1).
% big rotate right
balance(A, A1) :- left(A, B), right(A, R), left(B, L), right(B, C),
									 height(L, HL), height(C, HC), height(R, HR), height(B, HB),
									 2 is HB - HR, HC > HL, !, big_rotate_right(A, A1).