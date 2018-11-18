:- begin_tests(st_tokens_simple).

:- use_module(prolog/st/st_tokens).

test(text):-
    st_tokens(`abc`,
        _{ frontend: simple }, [text(`abc`)]).

test(out):-
    st_tokens(`{{= abc }}`,
        _{ frontend: simple }, [out(abc)]).

test(out_unescaped):-
    st_tokens(`{{- abc }}`,
        _{ frontend: simple }, [out_unescaped(abc)]).

test(end):-
    st_tokens(`{{ end }}`,
        _{ frontend: simple }, [end]).

test(else):-
    st_tokens(`{{ else }}`,
        _{ frontend: simple }, [else]).

test(include):-
    st_tokens(`{{ include file/name }}`,
        _{ frontend: simple }, [include(file/name)]).

test(include_var):-
    st_tokens(`{{ include file/name, var }}`,
        _{ frontend: simple }, [include(file/name, var)]).

test(dynamic_include):-
    st_tokens(`{{ dynamic_include var }}`,
        _{ frontend: simple }, [dynamic_include(var)]).

test(block):-
    st_tokens(`{{ block file/name }}`,
        _{ frontend: simple }, [block(file/name)]).

test(block_var):-
    st_tokens(`{{ block file/name, var }}`,
        _{ frontend: simple }, [block(file/name, var)]).

test(slot):-
    st_tokens(`{{ slot }}`,
        _{ frontend: simple }, [slot]).

test(if):-
    st_tokens(`{{ if x=1 }}`,
        _{ frontend: simple }, [if(x=1)]).

test(else_if):-
    st_tokens(`{{ else if x=2 }}`,
        _{ frontend: simple }, [else_if(x=2)]).

test(each_1):-
    st_tokens(`{{ each items, item }}`,
        _{ frontend: simple }, [each(items, item)]).

test(each_2):-
    st_tokens(`{{ each items, item, index }}`,
        _{ frontend: simple }, [each(items, item, index)]).

test(each_3):-
    st_tokens(`{{ each items, item, index, len }}`,
        _{ frontend: simple }, [each(items, item, index, len)]).

test(invalid):-
    catch((st_tokens(`{{ invalid`,
        _{ frontend: simple }, _), fail),
        error(invalid_instruction(_)), true).

test(nonground):-
    catch((st_tokens(`{{= A }}`,
        _{ frontend: simple }, _), fail),
        error(non_ground_expression(_)), true).

test(comment):-
    st_tokens(`{{% this is a comment }}`,
        _{ frontend: simple }, []).

:- end_tests(st_tokens_simple).
