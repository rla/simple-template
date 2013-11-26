:- module(st_render, [
    st_render/3,      % +Templ, +Data, +File
    st_render/4,      % +Templ, +Data, +Stream, +File
    st_render_file/2, % +File, +Data
    st_render_file/3, % +File, +Data, +Stream
    enable_cache/1
]).

:- use_module(library(readutil)).
:- use_module(st_parse).

:- dynamic(cache).
    
enable_cache(true):-
    (   cache
    ->  true
    ;   assertz(cache)).

enable_cache(false):-
    retractall(cache).
    
:- dynamic(template/2).

st_template(File, Templ):-
    with_mutex(st_template,
        st_template_unsafe(File, Templ)).

st_template_unsafe(File, Templ):-
    (   template(File, Templ)
    ->  true
    ;   read_file_to_codes(File, Codes, []),
        st_parse(Codes, Templ),
        assertz(template(File, Templ))).
    
st_template_cached(File, Templ):-
    (   cache
    ->  st_template(File, Templ)
    ;   read_file_to_codes(File, Codes, []),
        st_parse(Codes, Templ)).

st_render_file(File, Data):-
    current_output(Stream),
    st_render_file(File, Data, Stream).

st_render_file(File, Data, Stream):-
    absolute_file_name(File, Abs),
    st_template_cached(Abs, Templ),
    st_render(Templ, Data, Stream, Abs).

%% st_render(+Temp, +Data, +File) is det.
%
% Renders template with the given data.
% Writes output into current output.

st_render(Templ, Data, File):-
    current_output(Stream),
    st_render(Templ, Data, Stream, File).

st_render(Templ, Data, Stream, File):-
    st_render_scope(Templ, Data, Stream, File).
    
st_render_scope([out(Path)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Value),
    escape(Value, Escaped),
    write(Stream, Escaped),
    st_render_scope(Blocks, Scope, Stream, File).
    
st_render_scope([out_unescaped(Path)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Value),
    write(Stream, Value),
    st_render_scope(Blocks, Scope, Stream, File).

st_render_scope([block(each(Path, Var), Nested)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Values),
    (   is_list(Values)
    ->  st_render_scope_values(Values, Var, Nested, Scope, Stream, File),
        st_render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(path_in_each_not_list(Path)))).
    
st_render_scope([text(Text)|Blocks], Scope, Stream, File):- !,
    write(Stream, Text),
    st_render_scope(Blocks, Scope, Stream, File).
    
st_render_scope([include(Path)|Blocks], Scope, Stream, File):- !,
    file_directory_name(File, Dir),
    absolute_file_name(Path, Abs, [relative_to(Dir)]),
    atom_concat(Abs, '.html', AbsFile),
    st_render_file(AbsFile, Scope, Stream),
    st_render_scope(Blocks, Scope, Stream, File).
    
st_render_scope([Block|_], _, _, _):-
    throw(error(unknown_block(Block))).
    
st_render_scope([], _, _, _).

% Creates new scope entry and renders
% nested blocks.
    
st_render_scope_values([Value|Values], Var, Nested, Scope, Stream, File):-
    Entry =.. [Var, Value],
    st_render_scope(Nested, [Entry|Scope], Stream, File),
    st_render_scope_values(Values, Var, Nested, Scope, Stream, File).
    
st_render_scope_values([], _, _, _, _, _).

% Finds value from the given
% scope.
%
% Throws error(no_value(Atom))
% when the value is not found.
%
% Throws error(invalid_path(Path))
% when the path specifier is invalid.

scope_find(Atom, Scope, Value):-
    atom(Atom), !,
    Term =.. [Atom, Value],
    (   memberchk(Term, Scope)
    ->  true
    ;   throw(error(no_value(Atom)))).
    
scope_find(Base:Path, Scope, Value):-
    atom(Base), !,
    Term =.. [Base, SubScope],
    (   memberchk(Term, Scope)
    ->  scope_find(Path, SubScope, Value)
    ;   throw(error(no_value(Base)))).
    
scope_find(Path, _, _):-
    throw(error(invalid_path(Path))).

escape(Text, Html):-
    atom_codes(Text, Codes1),    
    phrase(escape_codes(Codes2), Codes1),
    atom_codes(Html, Codes2).
    
escape_codes(Codes) -->
    "<", !, escape_codes(Rest),
    { append("&lt;", Rest, Codes) }.

escape_codes(Codes) -->
    "&", !, escape_codes(Rest),
    { append("&amp;", Rest, Codes) }.
    
escape_codes(Codes) -->
    ">", !, escape_codes(Rest),
    { append("&gt;", Rest, Codes) }.
    
escape_codes(Codes) -->
    "'", !, escape_codes(Rest),
    { append("&#39;", Rest, Codes) }.
    
escape_codes(Codes) -->
    "\"", !, escape_codes(Rest),
    { append("&quot;", Rest, Codes) }.

escape_codes([Code|Codes]) -->
    [Code], !, escape_codes(Codes).

escape_codes([]) --> [].
