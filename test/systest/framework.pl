


testing_predicate(Predicate) :-
	current_predicate(Predicate/3),
	name(Predicate, PredName),
	append("f_test_", _, PredName).

test_name(TestPred, TestName) :-
	name(TestPred, PredName),
	append("f_test_", TestNameCharList, PredName),
	string_to_list(TestName, TestNameCharList).


write_io([]).
write_io([H|T]) :-
	write(H),
	write_io(T).

run_test(Pred) :-
	test_name(Pred, TestName),
	(call(Pred, _, [], OutIO) ->
		write_io(OutIO),
		write('.');
		nl, write('Test failed: '), write(TestName), nl, fail).

call_all([]).
call_all([P|Ps]) :-
	run_test(P),
	call_all(Ps).

test_it :-
	findall(P, testing_predicate(P), Ps),
	call_all(Ps),
	halt.