:- begin_tests(st_tokens_semblance).

:- use_module(prolog/st/st_tokens).

test(text):-
    st_tokens(`abc`,
        _{ frontend: semblance }, [text(`abc`)]).

test(out):-
    st_tokens(`{{ abc }}`,
        _{ frontend: semblance }, [out(abc)]).

test(out_unescaped):-
    st_tokens(`{% unescape abc %}`,
        _{ frontend: semblance }, [out_unescaped(abc)]).

test(end):-
    st_tokens(`{% end %}`,
        _{ frontend: semblance }, [end]).

test(else):-
    st_tokens(`{% else %}`,
        _{ frontend: semblance }, [else]).

test(include):-
    st_tokens(`{% include file/name %}`,
        _{ frontend: semblance }, [include(file/name)]).

test(include_var):-
    st_tokens(`{% include file/name, var %}`,
        _{ frontend: semblance }, [include(file/name, var)]).

test(dynamic_include):-
    st_tokens(`{% dynamic_include var %}`,
        _{ frontend: semblance }, [dynamic_include(var)]).

test(block):-
    st_tokens(`{% block file/name %}`,
        _{ frontend: semblance }, [block(file/name)]).

test(block_var):-
    st_tokens(`{% block file/name, var %}`,
        _{ frontend: semblance }, [block(file/name, var)]).

test(slot):-
    st_tokens(`{% slot %}`,
        _{ frontend: semblance }, [slot]).

test(if):-
    st_tokens(`{% if x=1 %}`,
        _{ frontend: semblance }, [if(x=1)]).

test(else_if):-
    st_tokens(`{% else if x=2 %}`,
        _{ frontend: semblance }, [else_if(x=2)]).

test(each_1):-
    st_tokens(`{% each items, item %}`,
        _{ frontend: semblance }, [each(items, item)]).

test(each_2):-
    st_tokens(`{% each items, item, index %}`,
        _{ frontend: semblance }, [each(items, item, index)]).

test(each_3):-
    st_tokens(`{% each items, item, index, len %}`,
        _{ frontend: semblance }, [each(items, item, index, len)]).

test(invalid):-
    catch((st_tokens(`{% invalid`,
        _{ frontend: semblance }, _), fail),
        error(invalid_instruction(_)), true).

test(nonground):-
    catch((st_tokens(`{{ A }}`,
        _{ frontend: semblance }, _), fail),
        error(non_ground_expression(_)), true).

test(comment):-
    st_tokens(`{# this is a comment #}`,
        _{ frontend: semblance }, []).

:- end_tests(st_tokens_semblance).
