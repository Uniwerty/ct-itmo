has_divisors(N, D) :- N > D, 0 is mod(N, D), !.
has_divisors(N, D) :- N > D, D1 is D + 1, has_divisors(N, D1).
composite(N) :- has_divisors(N, 2).
prime(N) :- \+ composite(N).

prime_divisors(N, R) :- prime_divisors(N, 2, R).
prime_divisors(1, K, []).
prime_divisors(N, K, R) :- 0 is mod(N, K), !, N1 is div(N, K), prime_divisors(N1, K, R0), R = [K | R0].
prime_divisors(N, K, R) :- K1 is K + 1, K1 =< N, prime_divisors(N, K1, R).

unique_prime_divisors(N, R) :- unique_prime_divisors(N, 2, R).
unique_prime_divisors(1, K, []).
unique_prime_divisors(N, K, R) :- 0 is mod(N, K), N1 is div(N, K), unique_prime_divisors(N1, K, [H | T]), H = K, !, R = [H | T].
unique_prime_divisors(N, K, R) :- 0 is mod(N, K), !, N1 is div(N, K), unique_prime_divisors(N1, K, R0), R = [K | R0].
unique_prime_divisors(N, K, R) :- K1 is K + 1, K1 =< N, unique_prime_divisors(N, K1, R).