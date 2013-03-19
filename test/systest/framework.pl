


testing_predicate(Predicate) :-
	current_predicate(Predicate/3),
	name(Predicate, PredName),
	append("f_test_", _, PredName).

test_name(TestPred, TestName) :-
	name(TestPred, PredName),
	append("f_test_", TestNameCharList, PredName),
	string_to_list(TestName, TestNameCharList).


write_io(_TestName, []).
write_io(TestName, [H|T]) :-
	nl, write(TestName), write(':'), nl,
	write('-------------------------------------------'), nl,
	do_write_io([H|T]),
	write('-------------------------------------------'), nl.

do_write_io([]).
do_write_io([H|T]) :-
	write(H),
	do_write_io(T).


run_test(Pred) :-
	test_name(Pred, TestName),
%	write(TestName), nl,
	(call(Pred, _, [], OutIO) ->
		write_io(TestName, OutIO),
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