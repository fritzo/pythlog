?- use_module(library(clpfd)).

pl_add(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L + R.
pl_add(pl_seq(Type, L), pl_seq(Type, R), pl_seq(Type, Z)) :-
	append(L, R, Z).

pli_assert(pl_bool(1), _).
pli_assert(pl_bool(0), Msg) :-
	write('AssertionError: '),
	pl_print([Msg], 1),
	fail.
pl_assert(Obj, Msg) :-
	pl_bool(Obj, Bool),
	pli_assert(Bool, Msg).


pl_bool(pl_bool(B), pl_bool(B)).
pl_bool(pl_int(0), pl_bool(0)).
pl_bool(pl_int(X), pl_bool(1)) :-
	X #\= 0.

pl_div(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L / R.

pl_eq(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #= R).
pl_eq(X, X, pl_bool(1)).

pl_gt(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #> R).

pl_gte(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #>= R).

pl_lt(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #< R).

pl_lte(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #=< R).

pl_mod(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L mod R.
pl_mult(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L * R.
pl_mult(pl_seq(Type, L), pl_int(R), pl_seq(Type, Z)) :-
	R #>= 0,
	pl_mult_seq([], L, R, Z).
pl_mult(pl_int(L), pl_seq(Type, R), pl_seq(Type, Z)) :-
	pl_mult(pl_seq(Type, R), pl_int(L), pl_seq(Type, Z)).

pl_mult_seq(Acc, _, 0, Acc).
pl_mult_seq(Acc, Seq, Times, Product) :-
	Times #> 0,
	append(Acc, Seq, NextAcc),
	NextTimes #= Times - 1,
	pl_mult_seq(NextAcc, Seq, NextTimes, Product).


pl_noteq(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #\= R).
pl_noteq(X, X, pl_bool(0)).
pl_noteq(pl_seq(Type, L), pl_seq(Type, R), pl_bool(1)) :-
	L \== R.

pl_pow(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L ^ R.

pli_print(pl_seq(str, CharList)) :-
	string_to_list(Str, CharList),
	write(Str).
pl_print([], 0).
pl_print([], 1) :-
	nl.	
pl_print([Obj|Objs], NewLine) :-
	f_str(Obj, Str),
	pli_print(Str),
	pl_print(Objs, NewLine).

pl_solve(pl_bool(1)).

pl_subscript_wrap_elem(str, Z, pl_seq(str, [Z])).
pl_subscript_wrap_elem(Type, Z, Z) :-
	Type \= str.
pl_subscript(pl_seq(Type, L), pl_int(R), E) :-
	R #>= 0,
	nth0(R, L, Z),
	pl_subscript_wrap_elem(Type, Z, E).
pl_subscript(pl_seq(Type, L), pl_int(R), E) :-
	R #< 0,
	length(L, Ll),
	I #= Ll + R,
	nth0(I, L, Z),
	pl_subscript_wrap_elem(Type, Z, E).



pl_sub(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L - R.


% builtins

f_len(pl_seq(Type, S), pl_int(N)) :-
	length(S, N),
	nth0(_, [str, list], Type).


f_str(pl_seq(str, S), pl_seq(str, S)).
f_str(pl_int(I), pl_seq(str, S)) :-
	integer(I),
	number_codes(I, S).
f_str(pl_seq(list, L), pl_seq(str, S)) :-
	fi_str_list(L, "[", Tmp),
	append(Tmp, "]", S).

f_str(pl_int(I), pl_seq(str, "?free")) :-
	var(I).

f_str(pl_object(Type, Attrs), pl_seq(str, Result)) :-
	name(Type, InternalName),
	length(StrPrefix, 2),
	append(StrPrefix, TypeName, InternalName),
	append(TypeName, "(", T0),
	fi_str_list(Attrs, T0, T1),
	append(T1, ")", Result).



fi_str_list([Obj], Acc, Result) :-
	f_str(Obj, pl_seq(str, ObjStr)),
	append(Acc, ObjStr, Result).
fi_str_list([Obj|Objs], Acc, Result) :-
	f_str(Obj, pl_seq(str, ObjStr)),
	append(Acc, ObjStr, T0),
	append(T0, ", ", NextAcc),
	fi_str_list(Objs, NextAcc, Result).
