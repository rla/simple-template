:- module(st_expr, [
    st_eval/3,         % + Expr, +Scope, -Result
    st_set_function/3, % +Name, +Arity, :Goal
    st_set_global/2    % +Name, +Value
]).

/** <module> Expression evaluator

Evaluates expression. Allows registering of
global constants and user-defined functions.
*/

:- use_module(library(error)).

:- dynamic(user_function/3).

:- meta_predicate(st_set_function(+, +, :)).

%! st_set_function(+Name, +Arity, :Goal) is det.
%
% Registers new function. Goal must have arity
% Arity + 1. Last argument of goal is used as
% the return value.

st_set_function(Name, Arity, Goal):-
    must_be(atom, Name),
    must_be(nonneg, Arity),
    must_be(nonvar, Goal),
    assert_function(Name, Arity, Goal).

assert_function(Name, Arity, Goal):-
    (   user_function(Name, Arity, Goal)
    ->  true
    ;   assertz(user_function(Name, Arity, Goal))).

:- dynamic(global/2).

%! st_set_global(+Name, +Value) is det.
%
% Sets the global value. Value must
% be ground. Overwrites the existing
% global with the same name.

st_set_global(Name, Value):-
    must_be(atom, Name),
    must_be(ground, Value),
    retractall(global(Name, _)),
    assertz(global(Name, Value)).

% Strings.

st_eval(String, _, String):-
    string(String), !.

% Numbers.

st_eval(Number, _, Number):-
    number(Number), !.

% Variables.

st_eval(Name, Scope, Value):-
    atom(Name), !,
    (   get_dict(Name, Scope, Value)
    ->  true
    ;   (   global(Name, Value)
        ->  true
        ;   throw(error(no_entry(Name))))).

% Boolean negation.

st_eval(\+(Cond), Scope, Value):- !,
    st_eval_bool(Cond, Scope, Bool),
    bool_neg(Bool, Value).

% Less-than.

st_eval(Left < Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   LeftValue < RightValue
    ->  Value = 1
    ;   Value = 0).

% Greater-than.

st_eval(Left > Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   LeftValue > RightValue
    ->  Value = 1
    ;   Value = 0).

% Equality.

st_eval(Left = Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   test_equality(LeftValue, RightValue)
    ->  Value = 1
    ;   Value = 0).

% Less-than-equal.

st_eval(Left =< Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   LeftValue =< RightValue
    ->  Value = 1
    ;   Value = 0).

% Greater-than-equal.

st_eval(Left >= Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   LeftValue >= RightValue
    ->  Value = 1
    ;   Value = 0).

% Logical and.

st_eval(','(Left, Right), Scope, Value):- !,
    st_eval_bool(Left, Scope, LeftValue),
    (   LeftValue = 0
    ->  Value = 0
    ;   st_eval_bool(Right, Scope, RightValue),
        (   RightValue = 0
        ->  Value = 0
        ;   Value = 1)).

% Logical or.

st_eval(';'(Left, Right), Scope, Value):- !,
    st_eval_bool(Left, Scope, LeftValue),
    (   LeftValue = 1
    ->  Value = 1
    ;   st_eval_bool(Right, Scope, RightValue),
        (   RightValue = 1
        ->  Value = 1
        ;   Value = 0)).

% Unary minus.

st_eval(-(Expr), Scope, Value):- !,
    st_eval(Expr, Scope, ExprValue),
    Value is -ExprValue.

% Unary plus.

st_eval(+(Expr), Scope, Value):- !,
    st_eval(Expr, Scope, ExprValue),
    Value is ExprValue.

% Scope get.

st_eval(Term, Scope, Value):-
    Term =.. ['.', Base, Name], !,
    st_eval(Base, Scope, Tmp),
    '.'(Tmp, Name, Value).

% Addition. String concatenation.

st_eval(Left + Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    (   number(LeftValue)
    ->  Value is LeftValue + RightValue
    ;   string_concat(LeftValue, RightValue, Value)).

% Substraction.

st_eval(Left - Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue - RightValue.

% Multiplication.

st_eval(Left * Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue * RightValue.

% Division.

st_eval(Left / Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue / RightValue.

% Modulo.

st_eval(Left mod Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue mod RightValue.

% Reminder.

st_eval(Left rem Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue rem RightValue.

% Integer division.

st_eval(Left // Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue // RightValue.

% Integer division (variant 2).

st_eval(Left div Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue div RightValue.

% Absolute value.

st_eval(abs(Expr), Scope, Abs):- !,
    st_eval(Expr, Scope, Value),
    Abs is abs(Value).

% Sign.

st_eval(sign(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is sign(Value).

% Max.

st_eval(max(Left, Right), Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is max(LeftValue, RightValue).

% Min.

st_eval(min(Left, Right), Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is min(LeftValue, RightValue).

% Random.

st_eval(random(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is random(Value).

% Round.

st_eval(round(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is round(Value).

% Truncate.

st_eval(truncate(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is truncate(Value).

% Floor.

st_eval(floor(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is floor(Value).

% Ceiling.

st_eval(ceiling(Expr), Scope, Sign):- !,
    st_eval(Expr, Scope, Value),
    Sign is ceiling(Value).

% Power.

st_eval(Left ** Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue ** RightValue.

% Power, alternative.

st_eval(Left ^ Right, Scope, Value):- !,
    st_eval(Left, Scope, LeftValue),
    st_eval(Right, Scope, RightValue),
    Value is LeftValue ^ RightValue.

% Conditional expressions.

st_eval(if(Cond, True, False), Scope, Value):- !,
    st_eval_bool(Cond, Scope, CondValue),
    (   CondValue = 0
    ->  st_eval(False, Scope, Value)
    ;   st_eval(True, Scope, Value)).

% "Literal" atom.

st_eval(atom(Atom), _, Atom):-
    atom(Atom), !.

% List literal

st_eval(List, Scope, Value):-
    is_list(List), !,
    st_eval_list(List, Scope, Value).

% Function calls.

st_eval(Compound, Scope, Value):-
    compound(Compound), !,
    function_call(Compound, Scope, Value).

st_eval_bool(Expr, Scope, Bool):-
    st_eval(Expr, Scope, Value),
    (   (Value = 0 ; Value = false)
    ->  Bool = 0
    ;   Bool = 1).

bool_neg(1, 0).
bool_neg(0, 1).

% Evaluates list of expressions.

st_eval_list([Expr|Exprs], Scope, [Value|Values]):-
    st_eval(Expr, Scope, Value),
    st_eval_list(Exprs, Scope, Values).

st_eval_list([], _, []).

% Performs coercion from atom to
% string when necessary.

test_equality(Value1, Value2):-
    (   string(Value1)
    ->  (   string(Value2)
        ->  Value1 = Value2
        ;   test_equality_string(Value1, Value2))
    ;   (   string(Value2)
        ->  test_equality_string(Value2, Value1)
        ;   Value1 = Value2)).

test_equality_string(String, Value):-
    (   string(Value)
    ->  String = Value
    ;   atom(Value),
        atom_string(Value, TestString),
        String = TestString).

function_call(Fun, Scope, Value):-
    Fun =.. [Name|Args],
    length(Args, Arity),
    (   user_function(Name, Arity, Goal)
    ->  st_eval_list(Args, Scope, Vals),
        append(Vals, [Value], GoalArgs),
        (   apply(Goal, GoalArgs)
        ->  true
        ;   throw(error(function_call_failed(GoalArgs))))
    ;   throw(error(no_function(Name/Arity)))).
