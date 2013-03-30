:- use_module(library(ordsets)).

% for Element in Iterable: ...
for(Iterable, Element, Line, Stack) :-
    getattr(Iterable, '__iter__', IterAttr),
    invoke(IterAttr, [], Iterator, Line, Stack),
    getattr(Iterator, '__next__', NextAttr),
    invoke(NextAttr, [], Element, Line, Stack).
% return Value
return(Return:_ReturnType, Value:_ValueType) :-
    copy_attr(Return, Value, frozen).
% Result = A Op B
binop(Op, A, B, Result, Line, Stack) :-
    concat_atom(['__', Op, '__'], FuncName),
    getattr(A, FuncName, Attr), !,
    invoke(Attr, [B], Result, Line, Stack).
binop(Op, A, B, Result, Line, Stack) :-
    concat_atom(['__r', Op, '__'], FuncName),
    getattr(B, FuncName, Attr), !,
    invoke(Attr, [A], Result, Line, Stack).
binop(Op, _:TA, _:TB, _, Line, _Stack) :-
    write(Line),
    write(': unsupported operation \''), write(Op), 
    write('\' for types '), write(TA), write(' and '), write(TB), nl,
    fail.

% A in B
cmp(in, A, B, Result, Line, Stack) :-
    getattr(A, '__contains__', Attr),
    invoke(Attr, [B], Result, Line, Stack).
% A == B
cmp(eq, A, B, Result, Line, Stack) :-
    getattr(A, '__eq__', Attr),
    invoke(Attr, [B], Result, Line, Stack).

% x = Target[Slice]
getitem(Target, Slice, Result, Line, Stack) :-
    getattr(Target, '__getitem__', Attr),
    invoke(Attr, [Slice], Result, Line, Stack).

% Target[Slice] = x
setitem(Target, Slice, Value, Line, Stack) :-
    getattr(Target, '__setitem__', Attr),
    invoke(Attr, [Slice, Value], _, Line, Stack).

% del Target[Slice]
delitem(Target, Slice, Line, Stack) :-
    getattr(Target, '__delitem__', Attr),
    invoke(Attr, [Slice], _, Line, Stack).

% invoke: breaks out of recursions.
invoke(_Callable, _Args, _Result, _Line, [Head|Stack]) :-
    member(Head, Stack).
% len(Arg)
invoke(func(len), [Arg], _:int, _, Stack) :-
    getattr(Arg, '__len__', Attr),
    invoke(Attr, [], _:int, _, Stack).

% Builtin type 'int'
invoke(method(_:int, '__add__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__sub__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__mult__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__div__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__mod__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__pow__'), [_:int], _:int, _, _Stack).
invoke(method(_:int, '__eq__'), [_:int], _:bool, _, _Stack).

% Builtin type 'list'
invoke(method(_:list, '__add__'), [_:list], _:list, _, _Stack).
invoke(method(_:list, '__contains__'), [_], _:bool, _, _Stack).
invoke(method(_:list, '__getitem__'), [_:int], E:_, Line, _Stack) :-
    freeze_object(E, Line).
invoke(method(S:list, '__setitem__'), [_:int, E:_], _:'NoneType', Line, _Stack) :-
    freeze_object(E, Line),
    mutate_object(S, Line).
invoke(method(S:list, '__delitem__'), [_:int], _:'NoneType', Line, _Stack) :-
    mutate_object(S, Line).
invoke(method(_:list, '__eq__'), [_:list], _:bool, _, _Stack).
invoke(method(_:list, '__len__'), [], _:int, _, _Stack).
invoke(method(_:list, '__iter__'), [], _:list_iterator, _, _Stack).
invoke(method(S:list, append), [E:_], _:'NoneType', Line, _Stack) :- 
    mutate_object(S, Line),
    freeze_object(E, Line).

% Builtin type 'list_iterator' (returned from list.__iter__)
invoke(method(_:list_iterator, '__next__'), [], Result:_, Line, _Stack) :-
    freeze_object(Result, Line).


getattr(Self:Type, Attr, method(Self:Type, Attr)) :-
    methods(Type, Methods),
    member(Attr, Methods).


methods(list,
    ['__add__', '__contains__', '__getitem__', '__setitem__', '__delitem__',
     '__eq__', '__len__', '__iter__', 'append']).
methods(list_iterator,
    ['__next__']).
methods(int,
    ['__add__', '__sub__', '__mult__', '__div__', '__mod__', '__pow__',
     '__eq__']).
getglobal(len, func(len)).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

freeze_object(Freezes, _Line) :-
    put_attr(Freezes, frozen, 1).
mutate_object(Mutates, _Line) :-
    put_attr(Mutates, mutated, 1).

copy_attr(Dst, Src, AttrName) :-
    get_attr(Src, AttrName, AttrValue), !,
    put_attr(Dst, AttrName, AttrValue).
copy_attr(_Dst, _Src, _AttName).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5

attrs2props(Var:_Type, Index, Props) :-
    (get_attr(Var, frozen, _) ->
        P0 = [f(Index)]
        ;
        P0 = []
    ),
    (get_attr(Var, mutated, _) ->
        P1 = [m(Index)]
        ;
        P1 = []
    ),
    append(P0, P1, Props).

args_props([], _, FinalProps, FinalProps).
args_props([Arg|CallArgs], Index, AccProps, FinalProps) :-
    attrs2props(Arg, Index, ArgProps),
    append(ArgProps, AccProps, NextAccProps),
    NextIndex is Index + 1,
    args_props(CallArgs, NextIndex, NextAccProps, FinalProps).

get_properties(Func, _NumArgs, Props) :-
    invoke(func(Func), CallArgs, CallResult, _, []),
    attrs2props(CallResult, 0, ResultProps),
    args_props(CallArgs, 1, ResultProps, Props).


write_properties(Func, NumArgs, Stream) :-
    findall(Props, get_properties(Func, NumArgs, Props), PropsList),
    length(PropsList, Length),
    (Length \= 0 ->
        (
         flatten(PropsList, Flatten), list_to_ord_set(Flatten, Set),
         write(Stream, Func), write(Stream, '/'),
         write(Stream, NumArgs), write(Stream, ' '),
         write_properties_set(Set, NumArgs, Stream)
        )
        ; true
    ).

filter_set(_, [], _, []).
filter_set(Name, [H|T], NumArgs, [Id|Rest]) :-
    H =.. [Name, Value], not(var(Value)), !,
    (Value > NumArgs -> Id = NumArgs; Id = Value),
    filter_set(Name, T,  NumArgs, Rest).
filter_set(Name, [_|T], NumArgs, Rest) :-
    filter_set(Name, T, NumArgs, Rest).

write_properties_set(Props, NumArgs, Stream) :-
    filter_set(m, Props, NumArgs, Ms),
    list_to_ord_set(Ms, OrdMs),
    write(Stream, OrdMs),
    write(Stream, ' '),
    filter_set(f, Props, NumArgs, Fs),
    list_to_ord_set(Fs, OrdFs),
    write(Stream, OrdFs),
    nl(Stream).

infer_function_properties([], _).
infer_function_properties([Func/Arity|Functions], Stream) :-
    write_properties(Func, Arity, Stream),
    infer_function_properties(Functions, Stream).

main(FileName) :-
    open(FileName, write, Stream),
    module_functions(Functions),
    infer_function_properties(Functions, Stream),
    close(Stream).


:- style_check(-singleton).
:- style_check(-discontiguous).
