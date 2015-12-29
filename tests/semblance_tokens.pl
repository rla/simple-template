:- begin_tests(semblance_tokens).

:- use_module(prolog/st/semblance_tokens).

test(text):-
    semblance_tokens(`abc`, [text(`abc`)]).

test(out):-
    semblance_tokens(`{{ abc }}`, [out(abc)]).

test(out_unescaped):-
    semblance_tokens(`{% unescape abc %}`, [out_unescaped(abc)]).

test(end):-
    semblance_tokens(`{% end %}`, [end]).

test(else):-
    semblance_tokens(`{% else %}`, [else]).

test(include):-
    semblance_tokens(`{% include file/name %}`, [include(file/name)]).

test(include_var):-
    semblance_tokens(`{% include file/name, var %}`, [include(file/name, var)]).

test(dynamic_include):-
    semblance_tokens(`{% dynamic_include var %}`, [dynamic_include(var)]).

test(if):-
    semblance_tokens(`{% if x=1 %}`, [if(x=1)]).

test(else_if):-
    semblance_tokens(`{% else if x=2 %}`, [else_if(x=2)]).

test(each_1):-
    semblance_tokens(`{% each items, item %}`, [each(items, item)]).

test(each_2):-
    semblance_tokens(`{% each items, item, index %}`, [each(items, item, index)]).

test(each_3):-
    semblance_tokens(`{% each items, item, index, len %}`, [each(items, item, index, len)]).

test(invalid):-
    catch((semblance_tokens(`{% invalid`, _), fail), error(invalid_instruction(_)), true).

test(nonground):-
    catch((semblance_tokens(`{{ A }}`, _), fail), error(non_ground_expression(_)), true).

test(comment):-
    semblance_tokens(`{# this is a comment #}`, []).

:- end_tests(semblance_tokens).
