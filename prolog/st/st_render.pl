:- module(st_render, [
    st_render_file/2,  % +File, +Data
    st_render_file/3,  % +File, +Data, +Stream
    st_render_codes/3, % +Codes, +Data, +File
    st_render_codes/4, % +Codes, +Data, +Stream, +File
    st_enable_cache/1, % +Bool
    st_set_function/3, % +Name, +Arity, :Goal
    st_set_global/2,   % +Name, +Value
    st_set_extension/1 % +Extension
]).

:- use_module(library(readutil)).
:- use_module(library(error)).
:- use_module(library(debug)).

:- use_module(st_parse).

:- dynamic(cache).
    
st_enable_cache(true):-
    (   cache
    ->  true
    ;   assertz(cache)).

st_enable_cache(false):-
    retractall(cache).
    
:- dynamic(cache/2).

template(File, Templ):-
    with_mutex(st_template,
        template_unsafe(File, Templ)).

template_unsafe(File, Templ):-
    (   cache(File, Templ)
    ->  true
    ;   read_file_to_codes(File, Codes, []),
        st_parse(Codes, Templ),
        assertz(cache(File, Templ))).
    
template_cached(File, Templ):-
    (   cache
    ->  template(File, Templ)
    ;   read_file_to_codes(File, Codes, []),
        st_parse(Codes, Templ)).

:- dynamic(fun/3).

:- meta_predicate(st_set_function(+, +, 2)).

%% st_set_function(+Name, +Arity, :Goal) is det.
%
% Registers new function. Goal must have arity
% Arity + 1. Last argument of goal is used as
% output.

st_set_function(Name, Arity, Goal):-
    must_be(atom, Name),
    must_be(nonneg, Arity),
    must_be(nonvar, Goal),
    assert_function(Name, Arity, Goal).
    
assert_function(Name, Arity, Goal):-
    (   fun(Name, Arity, Goal)
    ->  true
    ;   assertz(fun(Name, Arity, Goal))).
    
:- dynamic(global/2).

%% st_set_global(+Name, +Value) is det.
%
% Sets the global value. Value must
% be ground.

st_set_global(Name, Value):-
    must_be(atom, Name),
    must_be(ground, Value),
    Term =.. [Name, Value],
    retractall(global(Name, _)),
    assertz(global(Name, Term)).
    
append_globals(In, Out):-
    findall(Term, global(_, Term), Terms),
    append(In, Terms, Out).
    
:- dynamic(extension/1).

%% st_set_extension(+Atom) is det.
%
% Sets the file name extension.
% Calling it multiple times makes
% it use the last value.

st_set_extension(Ext):-
    must_be(atom, Ext),
    retractall(extension(_)),
    assertz(extension(Ext)).
    
current_extension(Ext):-
    (   extension(Ext)
    ->  true
    ;   throw(error(extension_not_set(Ext)))).

%% st_render_file(+File, +Data) is det.
%
% Renders given file with the given data.
% Calls st_render_file/3 with the current stream.
% File name is added current extension.
    
st_render_file(File, Data):-
    current_output(Stream),
    st_render_file(File, Data, Stream).

%% st_render_file(+File, +Data, +Stream) is det.
%
% Renders given file with the given data
% into the stream.
    
st_render_file(File, Data, Stream):-
    append_globals(Data, WithGlobals),
    render_file(File, WithGlobals, Stream).

%% st_render_codes(+Codes, +Data, +File) is det.
%
% Renders codes with the given data.
% Calls st_render_codes/3 with the current stream.
    
st_render_codes(Codes, Data, File):-
    current_output(Stream),
    st_render_codes(Codes, Data, Stream, File).

%% st_render_codes(+Codes, +Data, +Stream, +File) is det.
%
% Renders codes with the given data into the stream.
% File argument is used for resolving includes.
    
st_render_codes(Codes, Data, Stream, File):-
    st_parse(Codes, Templ),
    append_globals(Data, WithGlobals),
    render(Templ, WithGlobals, Stream, File).
    
render_file(File, Data, Stream):-
    absolute_file_name(File, Abs),
    current_extension(Ext),
    atom_concat(Abs, Ext, FullAbs),
    template_cached(FullAbs, Templ),
    render(Templ, Data, Stream, Abs).

%% st_render(+Temp, +Data, +File) is det.
%
% Renders template with the given data.
% Writes output into current output.

st_render(Temp, Data, File):-
    append_globals(Data, WithGlobals),
    render(Temp, WithGlobals, File).

render(Templ, Data, File):-
    current_output(Stream),
    render(Templ, Data, Stream, File).

render(Templ, Data, Stream, File):-
    must_be(ground, Data),
    render_scope(Templ, Data, Stream, File).
    
render_scope([out(Path)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Value),
    escape(Value, Escaped),
    write(Stream, Escaped),
    render_scope(Blocks, Scope, Stream, File).
    
render_scope([out_unescaped(Path)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Value),
    write(Stream, Value),
    render_scope(Blocks, Scope, Stream, File).

render_scope([block(each(Path, Var), Nested)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Values),
    (   is_list(Values)
    ->  render_scope_values(Values, Var, Nested, Scope, Stream, File),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(path_in_each_not_list(Path)))).
    
render_scope([block(each(Path, Var, IVar), Nested)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Values),
    (   is_list(Values)
    ->  render_scope_values(Values, Var, IVar, 0, Nested, Scope, Stream, File),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(path_in_each_not_list(Path)))).
    
render_scope([block(each(Path, Var, IVar, LVar), Nested)|Blocks], Scope, Stream, File):- !,
    scope_find(Path, Scope, Values),
    (   is_list(Values)
    ->  length(Values, Length),
        render_scope_values(Values, Var, IVar, 0, LVar, Length, Nested, Scope, Stream, File),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(path_in_each_not_list(Path)))).

render_scope([text(Text)|Blocks], Scope, Stream, File):- !,
    write(Stream, Text),
    render_scope(Blocks, Scope, Stream, File).
    
render_scope([include(Path)|Blocks], Scope, Stream, File):- !,
    resolve_file(Path, File, AbsFile),
    render_file(AbsFile, Scope, Stream),
    render_scope(Blocks, Scope, Stream, File).

% Handles include with subscope.
% Variables from the parent scope
% become unavailable in the included file.
    
render_scope([include(Path, Var)|Blocks], Scope, Stream, File):- !,
    resolve_file(Path, File, AbsFile),    
    scope_find(Var, Scope, Value),
    render_file(AbsFile, Value, Stream),
    render_scope(Blocks, Scope, Stream, File).
    
render_scope([call(Fun)|Blocks], Scope, Stream, File):- !,
    call_function(Fun, Scope, Stream),
    render_scope(Blocks, Scope, Stream, File).

render_scope([cond(if(Cond), True, False)|Blocks], Scope, Stream, File):- !,
    (   cond_eval(Cond, Scope)
    ->  render_scope(True, Scope, Stream, File)
    ;   render_scope(False, Scope, Stream, File)),
    render_scope(Blocks, Scope, Stream, File).

render_scope([Block|_], _, _, _):-
    throw(error(unknown_block(Block))).

render_scope([], _, _, _).

% Resolves included file Path
% aginst the current file File.

resolve_file(Path, File, Abs):-
    file_directory_name(File, Dir),
    absolute_file_name(Path, Abs, [relative_to(Dir)]).

call_function(Fun, Scope, Stream):-
    Fun =.. [Name|Args],
    length(Args, Arity),
    (   fun(Name, Arity, Goal)
    ->  maplist(eval_in_scope(Scope), Args, Vals),
        append(Vals, [Out], GoalArgs),
        apply(Goal, GoalArgs),
        write(Stream, Out)
    ;   throw(error(no_function(Name/Arity)))).

cond_eval(Left = Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue = RightValue.
    
cond_eval(Left \= Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue \= RightValue.
    
cond_eval(Left < Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue < RightValue.
    
cond_eval(Left > Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue > RightValue.
    
cond_eval(Left =< Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue =< RightValue.
    
cond_eval(Left >= Right, Scope):-
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue), !,
    LeftValue >= RightValue.
    
cond_eval(!(Cond), Scope):- !,
    (cond_eval(Cond, Scope) -> fail ; true).

cond_eval(','(Left, Right), Scope):-
    cond_eval(Left, Scope),
    cond_eval(Right, Scope).
    
cond_eval(';'(Left, _), Scope):-
    cond_eval(Left, Scope), !.
    
cond_eval(';'(_, Right), Scope):-
    cond_eval(Right, Scope), !.
    
cond_eval(Cond, _):-
    throw(error(cannot_evaluate_cond(Cond))).

eval_in_scope(Scope, Expr, Value):-
    expr_eval(Expr, Scope, Value).
    
expr_eval([], _, ''):- !.
    
expr_eval(Var, Scope, Value):-
    atom(Var), !,
    scope_find(Var, Scope, Value).

expr_eval(Num, _, Num):-
    number(Num), !.

% TODO special case for string
% concatenation?
% TODO all other Prolog operators as well.
    
expr_eval(Left + Right, Scope, Value):- !,
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue),
    Value is LeftValue + RightValue.
    
expr_eval(Left - Right, Scope, Value):- !,
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue),
    Value is LeftValue - RightValue.
    
expr_eval(Left / Right, Scope, Value):- !,
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue),
    Value is LeftValue / RightValue.
    
expr_eval(Left * Right, Scope, Value):- !,
    expr_eval(Left, Scope, LeftValue),
    expr_eval(Right, Scope, RightValue),
    Value is LeftValue * RightValue.

expr_eval([Code|Codes], _, Atom):-
    number(Code), !,
    atom_codes(Atom, [Code|Codes]).
    
expr_eval(Expr, _, _):-
    throw(error(cannot_evaluate_cond_expr(Expr))).

% Creates new scope entry and renders
% nested blocks.
    
render_scope_values([Value|Values], Var, Nested, Scope, Stream, File):-
    Entry =.. [Var, Value],
    render_scope(Nested, [Entry|Scope], Stream, File),
    render_scope_values(Values, Var, Nested, Scope, Stream, File).
    
render_scope_values([], _, _, _, _, _).

% Same as above for 'each' loop with the index variable.

render_scope_values([Value|Values], Var, IVar, Index, Nested, Scope, Stream, File):-
    Entry =.. [Var, Value],
    IEntry =.. [IVar, Index],
    render_scope(Nested, [Entry,IEntry|Scope], Stream, File),
    NIndex is Index + 1,
    render_scope_values(Values, Var, IVar, NIndex, Nested, Scope, Stream, File).
    
render_scope_values([], _, _, _, _, _, _, _).

% Same as above for 'each' loop with the index and length variable.

render_scope_values([Value|Values], Var, IVar, Index, LVar, Length, Nested, Scope, Stream, File):-
    Entry =.. [Var, Value],
    IEntry =.. [IVar, Index],
    LEntry =.. [LVar, Length],
    render_scope(Nested, [Entry,IEntry,LEntry|Scope], Stream, File),
    NIndex is Index + 1,
    render_scope_values(Values, Var, IVar, NIndex, LVar, Length, Nested, Scope, Stream, File).

render_scope_values([], _, _, _, _, _, _, _, _, _).

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
