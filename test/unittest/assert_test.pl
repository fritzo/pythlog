
main :-
	write('Starting: assert_test.pl'), nl,

	pl_assert(pl_bool(1), pl_seq(str, "hello"), _, _),
	pl_assert(pl_int(1), pl_seq(str, "hello"), _, _),
	pl_assert(pl_bool(1), pl_None, _, _),

	not( pl_assert(pl_int(0), pl_seq(str, "this is expected."), _, _) ),
	not( pl_assert(pl_bool(0), pl_seq(str, "this is expected"), _, _) ),

 	write('Done'), nl,
 	halt.
