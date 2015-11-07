:- module(st_file, [
    st_cached/2,           % +File, -Template
    st_resolve/3,          % +File, -AbsFile, +Options
    st_cache_put/2,        % +File, +Template
    st_cache_invalidate/0,
    st_resolve_include/3   % +Include, +File, -AbsFile
]).

/** <module> File handling

Handles file paths and caching.
*/

:- use_module(library(option)).

:- dynamic(template/2).

%! st_cached(+File, -Template) is semidet.
%
% Retrieves the cached template. Fails
% when no such template is cached.

st_cached(File, Template):-
    with_mutex(st_cache,
        cached_unsafe(File, Template)).

cached_unsafe(File, Template):-
    template(File, Template).

%! st_cache_invalidate is det.
%
% Purges all cache entries.

st_cache_invalidate:-
    retractall(template(_, _)).

%! st_resolve(+File, -AbsFile, +Options) is det.
%
% Resolves file name relative to current
% working directory. Adds extension.

st_resolve(File, AbsFile, Options):-
    option(extension(Ext), Options),
    absolute_file_name(File, Abs),
    atomic_list_concat([Abs, '.', Ext], AbsFile).

%! st_resolve_include(+Include, +File, -Abs) is det.
%
% Resolves included file against the current file.
% Assumes that File is absolute path.

st_resolve_include(Include, File, Abs):-
    absolute_file_name(Include, Abs, [relative_to(File)]).

%! st_cache_put(File, Template) is det.
%
% Puts the template into cache. Does
% nothing when caching is not enabled.

st_cache_put(File, Template):-
    with_mutex(st_cache,
        st_cache_put_unsafe(File, Template)).

st_cache_put_unsafe(File, Template):-
    (   template(File, _)
    ->  true
    ;   assertz(template(File, Template))).
