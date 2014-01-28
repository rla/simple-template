:- begin_tests(st_file).

:- use_module(prolog/st/st_file).

test(resolve):-
    st_set_extension(html),
    absolute_file_name('included.html', Abs),
    st_resolve(included, Abs).

test(resolve_include):-
    st_set_extension(html),
    st_resolve_include(path/included, '/path/to/file.html',
        '/path/to/path/included').

test(cache_disabled):-
    st_disable_cache,
    st_cache_put(test, []),
    (   st_cached(test, _)
    ->  fail
    ;   true),
    st_disable_cache.

test(cache_enabled):-
    st_enable_cache,
    st_cache_put(test, []),
    (   st_cached(test, [])
    ->  true
    ;   fail).

test(cache_cleaned):-
    st_enable_cache,
    st_cache_put(test, []),
    (   st_cached(test, [])
    ->  true
    ;   fail),
    st_disable_cache,
    (   st_cached(test, _)
    ->  fail
    ;   true).

:- end_tests(st_file).
