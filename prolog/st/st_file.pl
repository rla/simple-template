:- module(st_file, [
    st_enable_cache/0,
    st_disable_cache/0,
    st_cached/2,           % +File, -Template
    st_set_extension/1,    % +Atom
    st_resolve/2,          % +File, -AbsFile
    st_cache_put/2,        % +File, +Template
    st_cache_invalidate/0,
    st_resolve_include/3   % +Include, +File, -AbsFile
]).

/** <module> File handling

Handles file paths and caching.
*/

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

:- dynamic(extension/1).

%! st_set_extension(+Atom) is det.
%
% Sets the file name extension.
% Calling it multiple times makes
% it use the last value. Purges
% template cache.

st_set_extension(Ext):-
    must_be(atom, Ext),
    retractall(extension(_)),
    assertz(extension(Ext)),
    st_cache_invalidate.

current_extension(Ext):-
    (   extension(Ext)
    ->  true
    ;   throw(error(extension_not_set))).

:- dynamic(cache_enabled/0).

%! st_cache_invalidate is det.
%
% Purges all cache entries.

st_cache_invalidate:-
    retractall(template(_, _)).

%! st_enable_cache is det.
%
% Enables template caching.

st_enable_cache:-
    with_mutex(st_cache,
        enable_cache_unsafe).

enable_cache_unsafe:-
    (   cache_enabled
    ->  true
    ;   assertz(cache_enabled)).

%! st_disable_cache is det.
%
% Disables template caching.

st_disable_cache:-
    with_mutex(st_cache,
        disable_cache_unsafe).

disable_cache_unsafe:-
    retractall(cache_enabled),
    retractall(template(_, _)).

%! st_resolve(+File, -AbsFile) is det.
%
% Resolves file name relative to current
% working directory. Adds extension.

st_resolve(File, AbsFile):-
    absolute_file_name(File, Abs),
    current_extension(Ext),
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
    (   cache_enabled
    ->  (   template(File, _)
        ->  true
        ;   assertz(template(File, Template)))
    ;   true).
