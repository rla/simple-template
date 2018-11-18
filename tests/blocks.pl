:- begin_tests(st_parse).

:- use_module(prolog/st/st_parse).

test(empty):-
    st_parse(``, [], _{ frontend: simple }).

test(text):-
    st_parse(`abc`, [text("abc")], _{ frontend: simple }).

test(out):-
    st_parse(`{{= abc }}`, [out(abc)], _{ frontend: simple }).

test(out_unescaped):-
    st_parse(`{{- abc }}`, [out_unescaped(abc)], _{ frontend: simple }).

test(include):-
    st_parse(`{{ include file/name }}`, [include(file/name)], _{ frontend: simple }).

test(dynamic_include):-
    st_parse(`{{ dynamic_include var }}`, [dynamic_include(var)], _{ frontend: simple }).

test(block):-
    st_parse(`{{ block file/name }} a {{ end }}`, [block(file/name, [text(" a ")])], _{ frontend: simple }).

test(block_scoped):-
    st_parse(`{{ block file/name, var }} a {{ end }}`, [block(file/name, var, [text(" a ")])], _{ frontend: simple }).

test(if):-
    st_parse(`{{ if 1 }} a {{ end }}`, [if(1, [text(" a ")], [])], _{ frontend: simple }).

test(if_else):-
    st_parse(`{{ if 1 }} a {{ else }} b {{ end }}`,
        [if(1, [text(" a ")], [text(" b ")])], _{ frontend: simple }).

test(if_else_if):-
    st_parse(`{{ if 1 }} a {{ else if 2 }} b {{ end }}`,
        [if(1, [text(" a ")], [if(2, [text(" b ")], [])])], _{ frontend: simple }).

test(each_1):-
    st_parse(`{{ each items, item }} a {{ end }}`,
        [each(items, item, [text(" a ")])], _{ frontend: simple }).

test(each_2):-
    st_parse(`{{ each items, item, i }} a {{ end }}`,
        [each(items, item, i, [text(" a ")])], _{ frontend: simple }).

:- end_tests(st_parse).
