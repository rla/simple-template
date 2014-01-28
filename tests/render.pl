:- begin_tests(st_render).

:- use_module(prolog/st/st_parse).
:- use_module(prolog/st/st_render).
:- use_module(prolog/st/st_file).

:- meta_predicate(test_rendering(0, +)).

test_rendering(Goal, Expected):-
    with_output_to_codes(Goal, Codes),
    string_codes(String, Codes),
    assertion(String = Expected).

test(empty):-
    test_rendering(st_render_string("", [], dummy), "").

test(text):-
    test_rendering(st_render_string("text", [], dummy), "text").

test(variable_number):-
    test_rendering(st_render_string("{{= a }}", _{ a: 123 }, dummy), "123").

test(variable_atom):-
    test_rendering(st_render_string("{{= a }}", _{ a: abc }, dummy), "abc").

test(variable_string):-
    test_rendering(st_render_string("{{= a }}", _{ a: "abc" }, dummy), "abc").

test(escape):-
    test_rendering(st_render_string("{{= a }}", _{ a: '<' }, dummy), "&lt;").

test(nonescape):-
    test_rendering(st_render_string("{{- a }}", _{ a: '<' }, dummy), "<").

test(condition1_true):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ end }}", _{ a : 1 }, dummy), "t").

test(condition1_false):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ end }}", _{ a : 2 }, dummy), "").

test(condition2_true):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ else }}f{{ end }}", _{ a : 1 }, dummy), "t").

test(condition2_false):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ else }}f{{ end }}", _{ a : 2 }, dummy), "f").

test(condition3_true):-
    test_rendering(st_render_string("{{ if a=\"abc\" }}t{{ else }}f{{ end }}", _{ a : 'abc' }, dummy), "t").

test(condition3_false):-
    test_rendering(st_render_string("{{ if a=\"abc\" }}t{{ else }}f{{ end }}", _{ a : 'bcd' }, dummy), "f").

test(condition_else_if_1):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 1 }, dummy), "b1").

test(condition_else_if_2):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 2 }, dummy), "b2").

test(condition_else_if_3):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 3 }, dummy), "b3").

test(condition_else_if_4):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 4 }, dummy), "b4").

test(each):-
    test_rendering(st_render_string("{{ each items, item }}{{= item }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "123").

test(each_index):-
    test_rendering(st_render_string("{{ each items, item, i }}{{= item }}{{= i }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "102132").

test(each_index_length):-
    test_rendering(st_render_string("{{ each items, item, i, n }}{{= item }}{{= i }}{{= n }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "103213323").

test(include):-
    st_set_extension(html),
    test_rendering(st_render_string("{{ include tests/included }}", _{}, dummy), "i").

test(include_variable):-
    test_rendering(st_render_string("{{ include tests/included_variable }}", _{ a: 1 }, dummy), "1").

test(include_variable_scoped):-
    test_rendering(st_render_string("{{ include tests/included_variable, b }}",
        _{ b: _{ a: 1 } }, dummy), "1").

test(white_inline):-
    st_enable_strip_white,
    test_rendering(st_render_string("abc {{= a }} def",
        _{ a: 1 }, dummy), "abc 1 def").

test(white_unindent):-
    st_enable_strip_white,
    test_rendering(st_render_string("abc\n{{= a }}\n def",
        _{ a: 1 }, dummy), "abc\n1\ndef").

test(white_collapse_line_end):-
    st_enable_strip_white,
    test_rendering(st_render_string("abc\n{{= a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n1\ndef").

test(white_collapse_indent):-
    st_enable_strip_white,
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n1\ndef").

test(white_disabled):-
    st_disable_strip_white,
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n   \n   1\n\n def").

:- end_tests(st_render).
