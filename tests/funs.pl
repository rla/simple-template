:- begin_tests(st_funs).

:- use_module(prolog/st/st_expr).
:- use_module(prolog/st/st_funs).

test(encode_path):-
    st_eval(encode_path("/f oo/bar"), _{}, '/f%20oo/bar').

test(encode_query_value):-
    st_eval(encode_query_value("foo&bar"), _{}, 'foo%26bar').

test(encode_fragment):-
    st_eval(encode_fragment("foo#bar"), _{}, 'foo%23bar').

:- end_tests(st_funs).
