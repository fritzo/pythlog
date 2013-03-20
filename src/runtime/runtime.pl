?- use_module(library(clpfd)).

pl_add(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L + R.
pl_add(pl_seq(Type, L), pl_seq(Type, R), pl_seq(Type, Z)) :-
	append(L, R, Z).

pli_and(1, 1, 1).
pli_and(0, 1, 0).
pli_and(1, 0, 0).
pli_and(0, 0, 0).
pl_and(L, R, pl_bool(Z)) :-
	pl_bool(L, pl_bool(BL)),
	pl_bool(R, pl_bool(BR)),
	pli_and(BL, BR, Z).

pl_assert(Obj) :-
	pl_bool(Obj, pl_bool(1)).


pl_bool(pl_bool(B), pl_bool(B)).
pl_bool(pl_int(0), pl_bool(0)).
pl_bool(pl_int(X), pl_bool(1)) :-
	X #\= 0.

pl_div(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L / R.

pl_eq(X, X, pl_bool(1)).
pl_eq(X, Y, pl_bool(0)) :-
	X \= Y.
pl_eq(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #= R).

% TODO: Should only evaluate 'member' once! How?
pl_in(L, pl_seq(list, R), pl_bool(1)) :-
	member(L, R).
pl_in(L, pl_seq(list, R), pl_bool(0)) :-
	not(member(L, R)).
pl_in(L, pl_seq(str, R), pl_bool(1)) :-
	append(X, _, R), append(_, L, X).
pl_in(pl_seq(str, L), pl_seq(str, R), pl_bool(Z)) :-
	pli_sublist(L, R, Z).
% TODO: Should only evaluate 'get_assoc' once! How?
pl_in(Elem, pl_seq(dict, Assoc), pl_bool(1)) :-
	get_assoc(Elem, Assoc, _).
pl_in(Elem, pl_seq(dict, Assoc), pl_bool(0)) :-
	not(get_assoc(Elem, Assoc, _)).

% TODO: Should only evaluate 'prefix' once! How?
pli_sublist(Short, Long, 1) :-
    prefix(Short, Long).
pli_sublist(Short, Long, Z) :-
    not(prefix(Short, Long)),
    [_|NextLong] = Long,
    pli_sublist(Short, NextLong, Z).
pli_sublist(Short, [], 0) :-
    Short \= [].



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

pli_not(0, 1).
pli_not(1, 0).
pl_not(L, pl_bool(Z)) :-
	pl_bool(L, pl_bool(BL)),
	pli_not(BL, Z).

pl_noteq(pl_int(L), pl_int(R), pl_bool(Z)) :-
	Z #<==> (L #\= R).
pl_noteq(X, X, pl_bool(0)).
pl_noteq(pl_seq(Type, L), pl_seq(Type, R), pl_bool(1)) :-
	L \== R.

pl_or(L, R, pl_bool(Z)) :-
	pl_bool(L, pl_bool(BL)),
	pl_bool(R, pl_bool(BR)),
	Z #= BL #\/ BR.

pl_pow(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L ^ R.

pli_print(pl_seq(str, CharList), Backtrack, InIO, OutIO) :-
	string_to_list(Str, CharList),
	io_write(Str, Backtrack, InIO, OutIO).
pl_print([Obj], 0, Backtrack, InIO, OutIO) :-
	f_str(Obj, Str, _, _),
	pli_print(Str, Backtrack, InIO, OutIO).
pl_print([Obj], 1, Backtrack, InIO, OutIO) :-
	f_str(Obj, Str, _, _),
	pli_print(Str, Backtrack, InIO, NextIO),
	io_write('\n', Backtrack, NextIO, OutIO).
pl_print([Obj|Objs], NewLine, Backtrack, InIO, OutIO) :-
	Objs \= [],
	f_str(Obj, Str, _, _),
	pli_print(Str, Backtrack, InIO, IO_0),
	io_write(' ', Backtrack, IO_0, IO_1),
	pl_print(Objs, NewLine, Backtrack, IO_1, OutIO).

pl_subscript_wrap_elem(str, Z, pl_seq(str, [Z])).
pl_subscript_wrap_elem(Type, Z, Z) :-
	Type \= str.
pl_subscript(pl_seq(dict, Assoc), Key, Elem) :-
	get_assoc(Key, Assoc, Elem).
pl_subscript(pl_seq(Type, L), pl_int(R), Elem) :-
	Type \= dict,
	R #>= 0,
	nth0(R, L, Z),
	pl_subscript_wrap_elem(Type, Z, Elem).
pl_subscript(pl_seq(Type, L), pl_int(R), Elem) :-
	Type \= dict,
	R #< 0,
	length(L, Ll),
	I #= Ll + R,
	nth0(I, L, Z),
	pl_subscript_wrap_elem(Type, Z, Elem).



pl_sub(pl_int(L), pl_int(R), pl_int(Z)) :-
	Z #= L - R.


% builtins

f_dict(Elems, pl_seq(dict, Assoc)) :-
	list_to_assoc(Elems, Assoc).

f_keys(pl_seq(dict, Assoc), pl_seq(list, Keys), IO, IO) :-
	assoc_to_keys(Assoc, Keys).
f_len(pl_seq(list, List), pl_int(N), IO, IO) :-
	length(List, N).
f_len(pl_seq(str, Str), pl_int(N), IO, IO) :-
	length(Str, N).
f_len(pl_seq(dict, Assoc), pl_int(N), IO, IO) :-
	assoc_to_list(Assoc, List),
	length(List, N).

% Implement stringificaton of pairs of an association list.
f_repr(Var, pl_seq(str, "?object"), IO, IO) :-
	var(Var).
f_repr(Key-Value, pl_seq(str, Repr), IO, IO) :-
	f_repr(Key, pl_seq(str, KeyRepr), _, _),
	f_repr(Value, pl_seq(str, ValueRepr), _, _),
	append(KeyRepr, ":", Tmp),
	append(Tmp, ValueRepr, Repr).
f_repr(pl_seq(dict, Assoc), pl_seq(str, "?dict"), IO, IO) :-
	var(Assoc).
f_repr(pl_seq(dict, Assoc), pl_seq(str, Repr), IO, IO) :-
	not(var(Assoc)),
	assoc_to_list(Assoc, KeyValueList),
	fi_repr_list(KeyValueList, "{", Tmp),
	append(Tmp, "}", Repr).
f_repr(pl_seq(str, S), pl_seq(str, Repr), IO, IO) :-
	ground(S),
	append("'", S, T0),
	append(T0, "'", Repr).
f_repr(pl_seq(str, Var), pl_seq(str, "?str"), IO, IO) :-
	var(Var).
f_repr(pl_seq(str, Var), pl_seq(str, Str), IO, IO) :-
	not(var(Var)),
	not(ground(Var)),
	fi_str_non_ground(Var, Tmp0),
	append("'", Tmp0, Tmp1),
	append(Tmp1, "'", Str).
f_repr(pl_seq(list, L), pl_seq(str, Repr), IO, IO) :-
	fi_repr_list(L, "[", Tmp),
	append(Tmp, "]", Repr).
f_repr(pl_int(I), pl_seq(str, Repr), IO, IO) :-
	integer(I),
	number_codes(I, Repr).
f_repr(pl_int(I), pl_seq(str, "?int"), IO, IO) :-
	var(I).
f_repr(pl_None, pl_seq(str, "None"), IO, IO).
f_repr(pl_bool(V), pl_seq(str, "?bool"), IO, IO) :-
	var(V).
f_repr(pl_bool(1), pl_seq(str, "True"), IO, IO).
f_repr(pl_bool(0), pl_seq(str, "False"), IO, IO).
f_repr(pl_object(Type, Attrs), pl_seq(str, Result), IO, IO) :-
	name(Type, InternalName),
	length(StrPrefix, 2),
	append(StrPrefix, TypeName, InternalName),
	append(TypeName, "(", T0),
	fi_repr_list(Attrs, T0, T1),
	append(T1, ")", Result).

f_str(Var, pl_seq(str, "?object"), IO, IO) :-
	var(Var).
f_str(pl_seq(dict, A), S, IO, IO) :-
	f_repr(pl_seq(dict, A), S, _, _).
f_str(pl_seq(str, S), pl_seq(str, S), IO, IO) :-
	ground(S).
f_str(pl_seq(str, Var), pl_seq(str, "?str"), IO, IO) :-
	var(Var).
f_str(pl_seq(str, Var), pl_seq(str, Str), IO, IO) :-
	not(var(Var)),
	not(ground(Var)),
	fi_str_non_ground(Var, Str).
f_str(pl_int(I), S, IO, IO) :-
	f_repr(pl_int(I), S, _, _).
f_str(pl_seq(list, L), S, IO, IO) :-
	f_repr(pl_seq(list, L), S, _, _).
f_str(pl_None, S, IO, IO) :-
	f_repr(pl_None, S, _, _).
f_str(pl_bool(Z), S, IO, IO) :-
	f_repr(pl_bool(Z), S, IO, IO).
f_str(pl_object(Type, Attrs), pl_seq(str, Result), IO, IO) :-
	f_repr(pl_object(Type, Attrs), pl_seq(str, Result), IO, IO).

f_type(pl_bool(_), f_bool, IO, IO).
f_type(pl_int(_), f_int, IO, IO).
f_type(pl_seq(list, _), f_list, IO, IO).
f_type(pl_seq(str, _), f_str, IO, IO).
f_type(pl_seq(dict, _), f_dict, IO, IO).
f_type(pl_object(Type, _), Type, IO, IO).

f_values(pl_seq(dict, Assoc), pl_seq(list, Values), IO, IO) :-
	assoc_to_values(Assoc, Values).


fi_repr_list([], Result, Result).
fi_repr_list([Obj|Objs], Acc, Result) :-
	var(Objs),
	f_repr(Obj, pl_seq(str, ObjStr), _, _),
	append(Acc, ObjStr, T0),
	append(T0, ", ...", Result).
fi_repr_list([Obj], Acc, Result) :-
	f_repr(Obj, pl_seq(str, ObjStr), _, _),
	append(Acc, ObjStr, Result).
fi_repr_list([Obj|Objs], Acc, Result) :-
	not(var(Objs)),
	f_repr(Obj, pl_seq(str, ObjStr), _, _),
	append(Acc, ObjStr, T0),
	append(T0, ", ", NextAcc),
	fi_repr_list(Objs, NextAcc, Result).

fi_str_non_ground_char(Var, 63) :- % 64 = '?'
	var(Var).
fi_str_non_ground_char(Char, Char) :-
	not(var(Char)).
fi_str_non_ground([Val|L], [Char|"..."]) :-
	var(L),
	fi_str_non_ground_char(Val, Char).
fi_str_non_ground([Val|L], [Char|R]) :-
	not(var(L)),
	fi_str_non_ground_char(Val, Char),
	fi_str_non_ground(L, R).
fi_str_non_ground([], []).

% io handling

io_write(Str, 0, IO, IO) :- % write without backtrack
	write(Str).
io_write(Str, 1, InIO, OutIO) :- % write with backtrack
	append(InIO, [Str], OutIO). % TODO: Appending like this is inefficient


% list (prolog kind) handling

list_length_i([], L, L).
list_length_i([_|T], Acc, Length) :-
    not(var(T)),
    NextAcc #= Acc + 1,
    list_length_i(T, NextAcc, Length), !.
list_length_i([_|T], Acc, Length) :-
    var(T),
    Length #> Acc, !.

list_length(List, Length) :-
    not(var(Length)),
    length(List, Length).
list_length(List, Length) :-
	var(Length),
	list_length_i(List, 0, Length).
