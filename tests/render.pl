:- begin_tests(st_render).

:- use_module(prolog/st/st_parse).
:- use_module(prolog/st/st_render).
:- use_module(prolog/st/st_file).

% These add the stream argument to the call.

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
    test_rendering(st_render_string("{{ include tests/included }}", _{}, dummy), "i").

test(include_variable):-
    test_rendering(st_render_string("{{ include tests/included_variable }}", _{ a: 1 }, dummy), "1").

test(include_variable_scoped):-
    test_rendering(st_render_string("{{ include tests/included_variable, b }}",
        _{ b: _{ a: 1 } }, dummy), "1").

test(dynamic_include):-
    test_rendering(st_render_string("{{ dynamic_include file }}",
        _{ file: tests/included }, dummy), "i").

test(dynamic_include_variable):-
    test_rendering(st_render_string("{{ dynamic_include file }}",
        _{ a: 1, file: tests/included_variable }, dummy), "1").

test(dynamic_include_variable_scoped):-
    test_rendering(st_render_string("{{ dynamic_include file, b }}",
        _{ b: _{ a: 1 }, file: tests/included_variable }, dummy), "1").

test(white_inline):-
    test_rendering(st_render_string("abc {{= a }} def",
        _{ a: 1 }, dummy), "abc 1 def").

test(white_unindent):-
    test_rendering(st_render_string("abc\n{{= a }}\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(white_collapse_line_end):-
    test_rendering(st_render_string("abc\n{{= a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(white_collapse_indent):-
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy, _{ strip: true }), "abc\n1\ndef").

test(white_disabled):-
    test_rendering(st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy), "abc\n   \n   1\n\n def").

test(comment):-
    test_rendering(st_render_string("{{= a }}{{% this is a comment}}", _{ a: 123 }, dummy), "123").

test(invalid_frontend_tokenizer):-
    catch((st_render_string("abc\n   \n   {{= a }}\n\n def",
        _{ a: 1 }, dummy, dummy, _{ strip: true, frontend: dummy }), fail),
        error(type_error(_, _), _), true).

test(no_slot_content):-
    Options = _{ frontend: simple },
    catch((st_render_string("{{ slot }}",
        _{}, dummy, dummy, Options), fail),
        error(no_content_for_slot(_)), true).

test(slot_content_mock):-
    Slot = slot([text("a")], _{}),
    Options = _{ frontend: simple, '_slot': Slot },
    with_output_to_codes((
        current_output(Stream),
        st_render_string("{{ slot }}", _{}, Stream, dummy, Options)
    ), Codes),
    string_codes(String, Codes),
    assertion(String = "a").

test(block):-
    Options = _{ strip: true, frontend: simple },
    File = 'tests/current.html',
    Data = _{ message: "Hello" },
    with_output_to_codes((
        current_output(Stream),
        st_render_string("{{ block div_slot }}{{= message }}{{ end }}",
            Data, Stream, File, Options)
    ), Codes),
    string_codes(String, Codes),
    assertion(String = "<div>Hello</div>").

test(block_scoped):-
    Options = _{ strip: true, frontend: simple },
    File = 'tests/current.html',
    Data = _{ message: "Hello", block_scope: _{ who: "World" }},
    with_output_to_codes((
        current_output(Stream),
        st_render_string("{{ block div_slot_scoped, block_scope }}{{= message }}{{ end }}",
            Data, Stream, File, Options)
    ), Codes),
    string_codes(String, Codes),
    assertion(String = "<div>Hello World</div>").

:- end_tests(st_render).
