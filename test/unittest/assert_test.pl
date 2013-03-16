
main :-
	write('Starting: assert_test.pl'), nl,

	pl_assert(pl_bool(1), pl_seq(str, "hello")),
	pl_assert(pl_int(1), pl_seq(str, "hello")),
	pl_assert(pl_bool(1), pl_None),

	not( pl_assert(pl_int(0), pl_seq(str, "this is expected.")) ),
	not( pl_assert(pl_bool(0), pl_seq(str, "this is expected")) ),

 	write('Done'), nl,
 	halt.
