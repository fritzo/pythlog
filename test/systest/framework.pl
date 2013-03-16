


testing_predicate(Predicate) :-
	current_predicate(Predicate/1),
	name(Predicate, PredName),
	append("f_test_", _, PredName).

test_name(TestPred, TestName) :-
	name(TestPred, PredName),
	append("f_test_", TestNameCharList, PredName),
	string_to_list(TestName, TestNameCharList).


run_test(Pred) :-
	test_name(Pred, TestName),
	(call(Pred, _) ->
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