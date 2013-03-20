
main :-
	write('Starting: assert_test.pl'), nl,

	pl_assert(pl_bool(1)),
	pl_assert(pl_int(1)),

	not( pl_assert(pl_int(0)) ),
	not( pl_assert(pl_bool(0)) ),

 	write('Done'), nl,
 	halt.
