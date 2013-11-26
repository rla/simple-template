:- module(st_render, [
    st_render/2, % +Templ, +Data
    st_render/3,
    st_render_file/2,
    st_render_file/3
]).

:- use_module(library(readutil)).
:- use_module(st_parse).

st_render_file(File, Data):-
    current_output(Stream),
    st_render_file(File, Data, Stream).

st_render_file(File, Data, Stream):-
    read_file_to_codes(File, Codes, []),
    st_parse(Codes, Templ),
    st_render(Templ, Data, Stream).

%% st_render(+Temp, +Data) is det.
%
% Renders template with the given data.
% Writes output into current output.

st_render(Templ, Data):-
    current_output(Stream),
    st_render(Templ, Data, Stream).

st_render(Templ, Data, Stream):-
    st_render_scope(Templ, Data, Stream).
    
st_render_scope([out(Path)|Blocks], Scope, Stream):- !,
    scope_find(Path, Scope, Value),
    escape(Value, Escaped),
    write(Stream, Escaped),
    st_render_scope(Blocks, Scope, Stream).
    
st_render_scope([out_unescaped(Path)|Blocks], Scope, Stream):- !,
    scope_find(Path, Scope, Value),
    write(Stream, Value),
    st_render_scope(Blocks, Scope, Stream).

st_render_scope([block(each(Path, Var), Nested)|Blocks], Scope, Stream):- !,
    scope_find(Path, Scope, Values),
    (   is_list(Values)
    ->  st_render_scope_values(Values, Var, Nested, Scope, Stream),
        st_render_scope(Blocks, Scope, Stream)
    ;   throw(error(path_in_each_not_list(Path)))).
    
st_render_scope([text(Text)|Blocks], Scope, Stream):- !,
    write(Stream, Text),
    st_render_scope(Blocks, Scope, Stream).
    
st_render_scope([Block|_], _, _):-
    throw(error(unknown_block(Block))).
    
st_render_scope([], _, _).

% Creates new scope entry and renders
% nested blocks.
    
st_render_scope_values([Value|Values], Var, Nested, Scope, Stream):-
    Entry =.. [Var, Value],
    st_render_scope(Nested, [Entry|Scope], Stream),
    st_render_scope_values(Values, Var, Nested, Scope, Stream).
    
st_render_scope_values([], _, _, _, _).

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
