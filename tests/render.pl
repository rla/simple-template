:- begin_tests(st_render).

:- use_module(prolog/st/st_parse).
:- use_module(prolog/st/st_render).
:- use_module(prolog/st/st_file).

test_rendering(Call, Expected):-
    Call = st_render_string(Input, Data, File), !,
    with_output_to_codes((
        current_output(Stream),
        call(st_render_string(Input, Data, Stream, File, []))
    ), Codes),
    string_codes(String, Codes),
    assertion(String = Expected).

test_rendering(Call, Expected):-
    Call = st_render_string(Input, Data, File, Options),
    with_output_to_codes((
        current_output(Stream),
        call(st_render_string(Input, Data, Stream, File, Options))
    ), Codes),
    string_codes(String, Codes),
    assertion(String = Expected).

semblance_test_rendering(Call, Expected):-
    Call = st_render_string(Input, Data, File), !,
    with_output_to_codes((
        current_output(Stream),
        call(st_render_string(Input, Data, Stream, File, _{ frontend: semblance }))
    ), Codes),
    string_codes(String, Codes),
    assertion(String = Expected).

semblance_test_rendering(Call, Expected):-
    Call = st_render_string(Input, Data, File, Options),
    with_output_to_codes((
        current_output(Stream),
        call(st_render_string(Input, Data, Stream, File, Options))
    ), Codes),
    string_codes(String, Codes),
    assertion(String = Expected).

test(empty):-
    test_rendering(st_render_string("", [], dummy), "").

test(text):-
    test_rendering(st_render_string("text", [], dummy), "text").

test(variable_number):-
    test_rendering(st_render_string("{{= a }}", _{ a: 123 }, dummy), "123").

test(semblance_variable_number):-
    semblance_test_rendering(st_render_string("{{ a }}", _{ a: 123 }, dummy), "123").

test(variable_atom):-
    test_rendering(st_render_string("{{= a }}", _{ a: abc }, dummy), "abc").

test(semblance_variable_atom):-
    semblance_test_rendering(st_render_string("{{ a }}", _{ a: abc }, dummy), "abc").

test(variable_string):-
    test_rendering(st_render_string("{{= a }}", _{ a: "abc" }, dummy), "abc").

test(semblance_variable_string):-
    semblance_test_rendering(st_render_string("{{ a }}", _{ a: "abc" }, dummy), "abc").

test(escape):-
    test_rendering(st_render_string("{{= a }}", _{ a: '<' }, dummy), "&lt;").

test(semblance_escape):-
    semblance_test_rendering(st_render_string("{{ a }}", _{ a: '<' }, dummy), "&lt;").

test(nonescape):-
    test_rendering(st_render_string("{{- a }}", _{ a: '<' }, dummy), "<").

test(semblance_nonescape):-
    semblance_test_rendering(st_render_string("{% unescape a %}", _{ a: '<' }, dummy), "<").

test(condition1_true):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ end }}", _{ a : 1 }, dummy), "t").

test(semblance_condition1_true):-
    semblance_test_rendering(st_render_string("{% if a=1 %}t{% end %}", _{ a : 1 }, dummy), "t").

test(condition1_false):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ end }}", _{ a : 2 }, dummy), "").

test(semblance_condition1_false):-
    semblance_test_rendering(st_render_string("{% if a=1 %}t{% end %}", _{ a : 2 }, dummy), "").

test(condition2_true):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ else }}f{{ end }}", _{ a : 1 }, dummy), "t").

test(semblance_condition2_true):-
    semblance_test_rendering(st_render_string("{% if a=1 %}t{% else %}f{% end %}", _{ a : 1 }, dummy), "t").

test(condition2_false):-
    test_rendering(st_render_string("{{ if a=1 }}t{{ else }}f{{ end }}", _{ a : 2 }, dummy), "f").

test(semblance_condition2_false):-
    semblance_test_rendering(st_render_string("{% if a=1 %}t{% else %}f{% end %}", _{ a : 2 }, dummy), "f").

test(condition3_true):-
    test_rendering(st_render_string("{{ if a=\"abc\" }}t{{ else }}f{{ end }}", _{ a : 'abc' }, dummy), "t").

test(semblance_condition3_true):-
    semblance_test_rendering(st_render_string("{% if a=\"abc\" %}t{% else %}f{% end %}", _{ a : 'abc' }, dummy), "t").

test(condition3_false):-
    test_rendering(st_render_string("{{ if a=\"abc\" }}t{{ else }}f{{ end }}", _{ a : 'bcd' }, dummy), "f").

test(semblance_condition3_false):-
    semblance_test_rendering(st_render_string("{% if a=\"abc\" %}t{% else %}f{% end %}", _{ a : 'bcd' }, dummy), "f").

test(condition_else_if_1):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 1 }, dummy), "b1").

test(semblance_condition_else_if_1):-
    semblance_test_rendering(st_render_string("{% if a=1 %}b1{% else if a=2 %}b2{% else if a=3 %}b3{% else %}b4{% end %}",
        _{ a : 1 }, dummy), "b1").

test(condition_else_if_2):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 2 }, dummy), "b2").

test(semblance_condition_else_if_2):-
    semblance_test_rendering(st_render_string("{% if a=1 %}b1{% else if a=2 %}b2{% else if a=3 %}b3{% else %}b4{% end %}",
        _{ a : 2 }, dummy), "b2").

test(condition_else_if_3):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 3 }, dummy), "b3").

test(semblance_condition_else_if_3):-
    semblance_test_rendering(st_render_string("{% if a=1 %}b1{% else if a=2 %}b2{% else if a=3 %}b3{% else %}b4{% end %}",
        _{ a : 3 }, dummy), "b3").

test(condition_else_if_4):-
    test_rendering(st_render_string("{{ if a=1 }}b1{{ else if a=2 }}b2{{ else if a=3 }}b3{{ else }}b4{{ end }}",
        _{ a : 4 }, dummy), "b4").

test(semblance_condition_else_if_4):-
    semblance_test_rendering(st_render_string("{% if a=1 %}b1{% else if a=2 %}b2{% else if a=3 %}b3{% else %}b4{% end %}",
        _{ a : 4 }, dummy), "b4").

test(each):-
    test_rendering(st_render_string("{{ each items, item }}{{= item }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "123").

test(semblance_each):-
    semblance_test_rendering(st_render_string("{% each items, item %}{{ item }}{% end %}",
        _{ items: [1,2,3] }, dummy), "123").

test(each_index):-
    test_rendering(st_render_string("{{ each items, item, i }}{{= item }}{{= i }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "102132").

test(semblance_each_index):-
    semblance_test_rendering(st_render_string("{% each items, item, i %}{{ item }}{{ i }}{% end %}",
        _{ items: [1,2,3] }, dummy), "102132").

test(each_index_length):-
    test_rendering(st_render_string("{{ each items, item, i, n }}{{= item }}{{= i }}{{= n }}{{ end }}",
        _{ items: [1,2,3] }, dummy), "103213323").

test(semblance_each_index_length):-
    semblance_test_rendering(st_render_string("{% each items, item, i, n %}{{ item }}{{ i }}{{ n }}{% end %}",
        _{ items: [1,2,3] }, dummy), "103213323").

test(include):-
    test_rendering(st_render_string("{{ include tests/included }}", _{}, dummy), "i").

test(semblance_include):-
    semblance_test_rendering(st_render_string("{% include tests/included %}", _{}, dummy), "i").

test(include_variable):-
    test_rendering(st_render_string("{{ include tests/included_variable }}", _{ a: 1 }, dummy), "1").

test(semblance_include_variable):-
    semblance_test_rendering(st_render_string("{% include tests/semblance_included_variable %}", _{ a: 1 }, dummy), "1\n").

test(include_variable_scoped):-
    test_rendering(st_render_string("{{ include tests/included_variable, b }}",
        _{ b: _{ a: 1 } }, dummy), "1").

test(semblance_include_variable_scoped):-
    semblance_test_rendering(st_render_string("{% include tests/semblance_included_variable, b %}",
        _{ b: _{ a: 1 } }, dummy), "1\n").

test(dynamic_include):-
    test_rendering(st_render_string("{{ dynamic_include file }}",
        _{ file: tests/included }, dummy), "i").

test(semblance_dynamic_include):-
    semblance_test_rendering(st_render_string("{% dynamic_include file %}",
        _{ file: tests/included }, dummy), "i").

test(dynamic_include_variable):-
    test_rendering(st_render_string("{{ dynamic_include file }}",
        _{ a: 1, file: tests/included_variable }, dummy), "1").

test(semblance_dynamic_include_variable):-
    semblance_test_rendering(st_render_string("{% dynamic_include file %}",
        _{ a: 1, file: tests/semblance_included_variable }, dummy), "1\n").

test(dynamic_include_variable_scoped):-
    test_rendering(st_render_string("{{ dynamic_include file, b }}",
        _{ b: _{ a: 1 }, file: tests/included_variable }, dummy), "1").

test(semblance_dynamic_include_variable_scoped):-
    semblance_test_rendering(st_render_string("{% dynamic_include file, b %}",
        _{ b: _{ a: 1 }, file: tests/semblance_included_variable }, dummy), "1\n").

test(white_inline):-
    test_rendering(st_render_string("abc {{= a }} def",
        _{ a: 1 }, dummy), "abc 1 def").

test(semblance_white_inline):-
    semblance_test_rendering(st_render_string("abc {{ a }} def",
        _{ a: 1 }, dummy), "abc 1 def").

test(white_unindent):-
    test_rendering(st_render_string("abc\n{{= a }}\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(semblance_white_unindent):-
    semblance_test_rendering(st_render_string("abc\n{{ a }}\n def",
        _{ a: 1 }, dummy, _{ strip: true, frontend: semblance }), "abc\n1\ndef").

test(white_collapse_line_end):-
    test_rendering(st_render_string("abc\n{{= a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(semblance_white_collapse_line_end):-
    semblance_test_rendering(st_render_string("abc\n{{ a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true, frontend: semblance }), "abc\n1\ndef").

test(white_collapse_indent):-
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(semblance_white_collapse_indent):-
    semblance_test_rendering(st_render_string("abc\n   \n   {{ a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true, frontend: semblance }), "abc\n1\ndef").

test(white_disabled):-
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n   \n   1\n\n def").

test(semblance_white_disabled):-
    semblance_test_rendering(st_render_string("abc\n   \n   {{ a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n   \n   1\n\n def").

test(comment):-
    test_rendering(st_render_string("{{= a }}{{% this is a comment}}", _{ a: 123 }, dummy), "123").

test(semblance_comment):-
    semblance_test_rendering(st_render_string("{{ a }}{# this is a comment #}", _{ a: 123 }, dummy), "123").

test(invalid_frontend_tokenizer):-
    catch((st_render_string("abc\n   \n   {{= a }}\n\n def",
                            _{ a: 1 }, dummy, dummy, _{ strip: true, frontend: dummy }), fail),
          error(invalid_frontend_tokenizer(_)), true).

:- end_tests(st_render).
