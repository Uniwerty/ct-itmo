variable(Name, variable(Name)).
const(Value, const(Value)).
op_add(A, B, operation(op_add, A, B)).
op_subtract(A, B, operation(op_subtract, A, B)).
op_multiply(A, B, operation(op_multiply, A, B)).
op_divide(A, B, operation(op_divide, A, B)).
op_negate(A, operation(op_negate, A)).

lookup(K, [(K, V) | _], V).
lookup(K, [_ | T], V) :- lookup(K, T, V).

evaluate(const(Value), _, Value).
evaluate(variable(Name), Variables, Result) :- lookup(Name, Variables, Result).
evaluate(operation(Op, A, B), Variables, Result) :- evaluate(A, Variables, AV),
																								evaluate(B, Variables, BV),
																								binary(Op, AV, BV, Result).
evaluate(operation(Op, A), Variables, Result) :- evaluate(A, Variables, AV),
																						 unary(Op, AV, Result).

binary(op_add, A, B, R) :- R is A + B.
binary(op_subtract, A, B, R) :- R is A - B.
binary(op_multiply, A, B, R) :- R is A * B.
binary(op_divide, A, B, R) :- R is A / B.
unary(op_negate, A, R) :- R is -A.

:- load_library('alice.tuprolog.lib.DCGLibrary').

concat([], B, B).
concat([H | T], B, [H | R]) :- concat(T, B, R).

expr_p(variable(Name)) --> ws_star(L1), {ws(L1)}, [Name], ws_star(L2), {ws(L2)}, { member(Name, ['x', 'y', 'z']) }.
expr_p(const(Value)) --> {nonvar(Value), Value < 0}, ws_star(L1), {ws(L1)},
													{number_chars(Value, Chars), concat(['-'], Chars1, C0), concat(C0, ['.'], C), concat(C, Chars2, Chars)},
													['-'],
													digits_p(Chars1),
													['.'],
													digits_p(Chars2),
													ws_star(L2), {ws(L2)},
													{Chars = [_ | _], Chars2 = [_ | _]}.								
expr_p(const(Value)) --> {nonvar(Value)}, ws_star(L1), {ws(L1)},
													{number_chars(Value, Chars), concat(Chars1, ['.'], C), concat(C, Chars2, Chars)},
													digits_p(Chars1),
													['.'],
													digits_p(Chars2),
													ws_star(L2), {ws(L2)},
													{Chars = [_ | _], Chars2 = [_ | _]}.
expr_p(const(Value)) --> {var(Value)},
													ws_star(L1), {ws(L1)},
													digits_p(Chars),
													['.'],
													digits_p(Chars2),
													ws_star(L2), {ws(L2)},
													{Chars = [_ | _], Chars2 = [_ | _]},
													{concat(Chars, ['.'], C), concat(C, Chars2, C2), print(C2), atom_chars(S, C2), num_atom(Value, S)}.
expr_p(const(Value)) --> {var(Value)},
													ws_star(L1), {ws(L1)},
													['-'],
													digits_p(Chars),
													['.'],
													digits_p(Chars2),
													ws_star(L2), {ws(L2)},
													{Chars = [_ | _], Chars2 = [_ | _]},
													{concat(Chars, ['.'], C), concat(C, Chars2, C2), print(C2), atom_chars(S, C2), num_atom(Val, S), Value is -Val}.

expr_p(operation(Op, A, B)) -->	 ws_star(L1), {ws(L1)}, ['('],
																 ws_star(L2), {ws(L2)},
																 expr_p(A),
																 ws_plus(L3), {ws(L3)},
																 expr_p(B),
																 ws_plus(L4), {ws(L4)},
																 binary_p(Op),
																 ws_star(L5), {ws(L5)}, [')'],
																 ws_star(L6), {ws(L6)}.
expr_p(operation(Op, A)) --> ws_star(L1), {ws(L1)}, ['('],
														  ws_star(L2), {ws(L2)},
														  expr_p(A),
														  ws_plus(L3), {ws(L3)},
														  unary_p(Op),
														  ws_star(L4), {ws(L4)}, [')'],
														  ws_star(L5), {ws(L5)}.

digits_p([]) --> [].
digits_p([H | T]) -->
				{ member(H, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) },
				[H], digits_p(T).

ws_star([]) --> [].
ws_star([' ' | T]) --> [' '], ws_star(T).
ws_plus([' ' | T]) --> [' '], ws_star(T).
ws([]).
ws([' ' | T]) :- ws(T).

binary_p(op_add) --> ['+'].
binary_p(op_subtract) --> ['-'].
binary_p(op_multiply) --> ['*'].
binary_p(op_divide) --> ['/'].
unary_p(op_negate) --> ['n','e','g','a','t','e'].

suffix_str(Expression, Atom) :- ground(Expression),
																 phrase(expr_p(Expression), S), !,
																 atom_chars(Atom, S).
suffix_str(Expression, Atom) :- atom(Atom),
																 atom_chars(Atom, S),
																 phrase(expr_p(Expression), S), !.