:- begin_tests(st_parse).

:- use_module(prolog/st/st_parse).

test(empty):-
    st_parse(``, [], []).

test(text):-
    st_parse(`abc`, [text("abc")], []).

test(out):-
    st_parse(`{{ abc }}`, [out(abc)], []).

test(out_unescaped):-
    st_parse(`{% unescape abc %}`, [out_unescaped(abc)], []).

test(include):-
    st_parse(`{% include file/name %}`, [include(file/name)], []).

test(dynamic_include):-
    st_parse(`{% dynamic_include var %}`, [dynamic_include(var)], []).

test(if):-
    st_parse(`{% if 1 %} a {% end %}`, [if(1, [text(" a ")], [])], []).

test(if_else):-
    st_parse(`{% if 1 %} a {% else %} b {% end %}`,
        [if(1, [text(" a ")], [text(" b ")])], []).

test(if_else_if):-
    st_parse(`{% if 1 %} a {% else if 2 %} b {% end %}`,
        [if(1, [text(" a ")], [if(2, [text(" b ")], [])])], []).

test(each_1):-
    st_parse(`{% each items, item %} a {% end %}`,
        [each(items, item, [text(" a ")])], []).

test(each_2):-
    st_parse(`{% each items, item, i %} a {% end %}`,
        [each(items, item, i, [text(" a ")])], []).

test(each_3):-
    st_parse(`{% each items, item, i, len %} a {% end %}`,
        [each(items, item, i, len, [text(" a ")])], []).

:- end_tests(st_parse).
