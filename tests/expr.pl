:- begin_tests(st_expr).

:- use_module(prolog/st/st_expr).
:- use_module(point).

:- st_set_function(myfun, 2, myfun).

myfun(A, B, Out):-
    Out is A + B.

:- st_set_global(myglobal, 42).

test(string):-
    st_eval("string", _{}, "string").

test(number):-
    st_eval(123, _{}, 123).

test(variable):-
    st_eval(a, _{ a: 123 }, 123).

test(list_empty):-
    st_eval([], _{}, []).

test(list_non_empty):-
    st_eval([a], _{ a: 123 }, [123]).

test(bool_neg_1):-
    st_eval(\+ 1, _{}, 0).

test(bool_neg_2):-
    st_eval(\+ 2, _{}, 0).

test(bool_neg_3):-
    st_eval(\+ "a", _{}, 0).

test(bool_neg_4):-
    st_eval(\+ 0, _{}, 1).

test(bool_and_1):-
    st_eval((0, 0), _{}, 0).

test(bool_and_2):-
    st_eval((1, 0), _{}, 0).

test(bool_and_3):-
    st_eval((0, 1), _{}, 0).

test(bool_and_4):-
    st_eval((1, 1), _{}, 1).

test(bool_or_1):-
    st_eval((0; 0), _{}, 0).

test(bool_or_2):-
    st_eval((1; 0), _{}, 1).

test(bool_or_3):-
    st_eval((0; 1), _{}, 1).

test(bool_or_4):-
    st_eval((1; 1), _{}, 1).

test(less_than_true):-
    st_eval(1 < 2, _{}, 1).

test(less_than_false):-
    st_eval(2 < 1, _{}, 0).

test(greater_than_true):-
    st_eval(2 > 1, _{}, 1).

test(greater_than_false):-
    st_eval(1 > 2, _{}, 0).

test(equality_1):-
    st_eval(1 = 1, _{}, 1).

test(equality_2):-
    st_eval(2 = 1, _{}, 0).

test(equality_3):-
    st_eval(a = a, _{ a: 123 }, 1).

test(equality_4):-
    st_eval(a = "abc", _{ a: abc }, 1).

test(equality_5):-
    st_eval("abc" = a, _{ a: abc }, 1).

test(equality_6):-
    st_eval("abc" = 1, _{}, 0).

test(equality_7):-
    st_eval(1 = "abc", _{}, 0).

test(inequality_1):-
    st_eval(1 \= 0, _{}, 1).

test(inequality_2):-
    st_eval(1 \= 1, _{}, 0).

test(inequality_3):-
    st_eval(atom(a) \= "b", _{}, 1).

test(inequality_4):-
    st_eval(atom(a) \= "a", _{}, 0).

test(less_than_equal_true):-
    st_eval(1 =< 2, _{}, 1).

test(less_than_equal_false):-
    st_eval(2 =< 1, _{}, 0).

test(greater_than_equal_true):-
    st_eval(2 >= 1, _{}, 1).

test(greater_than_equal_false):-
    st_eval(1 >= 2, _{}, 0).

test(unary_minus):-
    st_eval(-(1), _{}, -1).

test(unary_plus):-
    st_eval(+(1), _{}, 1).

test(scope_get):-
    Term =.. ['.', a, b],
    st_eval(Term, _{ a:_{ b: 123 } }, 123).

test(scope_get_nested):-
    Inner =.. ['.', a, b],
    Term =.. ['.', Inner, c],
    st_eval(Term, _{ a:_{ b: _{ c: 123 } } }, 123).

test(dict_funcall):-
    Dict = point{x:1, y:2},
    Term =.. ['.', a, multiply(3)],
    st_eval(Term, _{ a: Dict }, point{x:3,y:6}).

test(addition):-
    st_eval(2 + 1, _{}, 3).

test(concat_atom):-
    st_eval(atom(abc) + 1, _{}, abc1).

test(concat_string):-
    st_eval("abc" + 1, _{}, "abc1").

test(substraction):-
    st_eval(3 - 1, _{}, 2).

test(multiplication):-
    st_eval(3 * 2, _{}, 6).

test(division):-
    st_eval(6 / 2, _{}, 3).

test(modulo):-
    st_eval(6 mod 4, _{}, 2).

test(reminder):-
    st_eval(3 rem 2, _{}, 1).

test(integer_division_1):-
    st_eval(7 // 2, _{}, 3).

test(integer_division_2):-
    st_eval(7 div 2, _{}, 3).

test(abs_1):-
    st_eval(abs(-3), _{}, 3).

test(abs_2):-
    st_eval(abs(3), _{}, 3).

test(sign_1):-
    st_eval(sign(-3), _{}, -1).

test(sign_2):-
    st_eval(sign(3), _{}, 1).

test(max):-
    st_eval(max(2, 3), _{}, 3).

test(min):-
    st_eval(min(2, 3), _{}, 2).

test(random):-
    st_eval(random(10), _{}, I),
    number(I).

test(round):-
    st_eval(round(3.9), _{}, 4).

test(truncate):-
    st_eval(truncate(3.9), _{}, 3).

test(floor):-
    st_eval(floor(3.9), _{}, 3).

test(ceiling):-
    st_eval(ceiling(3.9), _{}, 4).

test(power):-
    st_eval(3 ** 2, _{}, 9).

test(power):-
    st_eval(3 ^ 2, _{}, 9).

test(function):-
    st_eval(myfun(2, 3), _{}, 5).

test(function_composition):-
    st_eval(myfun(myfun(2, 3), 4), _{}, 9).

test(global):-
    st_eval(myglobal, _{}, 42).

test(global_shadow):-
    st_eval(myglobal, _{ myglobal: 24 }, 24).

test(condition_true):-
    st_eval(if(2, 3, 4), _{}, 3).

test(condition_false_1):-
    st_eval(if(0, 3, 4), _{}, 4).

test(condition_false_2):-
    st_eval(if(a, 3, 4), _{ a: false }, 4).

:- end_tests(st_expr).
