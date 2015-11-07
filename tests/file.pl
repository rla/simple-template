:- begin_tests(st_file).

:- use_module(prolog/st/st_file).

test(resolve):-
    absolute_file_name('included.html', Abs),
    st_resolve(included, Abs, [extension(html)]).

test(resolve_txt):-
    absolute_file_name('included.txt', Abs),
    st_resolve(included, Abs, [extension(txt)]).

test(resolve_include):-
    st_resolve_include(path/included, '/path/to/file.html',
        '/path/to/path/included').

test(cache_invalidate):-
    st_cache_put(test, []),
    (   st_cached(test, [])
    ->  true
    ;   fail),
    st_cache_invalidate,
    (   st_cached(test, _)
    ->  fail
    ;   true).

:- end_tests(st_file).
