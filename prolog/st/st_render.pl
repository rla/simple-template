:- module(st_render, [
    st_render_string/5, % +String, +Data, +Stream, +File, +Options
    st_render_file/4,   % +File, +Data, +Stream, +Options
    st_render_codes/5   % +Codes, +Data, +Stream, +File, +Options
]).

/** <module> Template renderer

Turns template together with data into output.
*/

:- use_module(library(readutil)).
:- use_module(library(error)).
:- use_module(library(debug)).
:- use_module(library(option)).

:- use_module(st_parse).
:- use_module(st_expr).
:- use_module(st_file).
:- use_module(st_escape).
:- use_module(st_funs).

% Default options for the renderer.

default_options(_{
    encoding: utf8,
    extension: html,
    cache: false,
    strip: false,
    frontend: simple,
    undefined: error
}).

% Merges the given options with
% default ones.

merge_defaults(Options, Actual):-
    default_options(Defaults),
    merge_options(Options, Defaults, Actual).

%! st_render_string(+String, +Data, +Stream, +File, +Options) is det.
%
% Renders given string with the given data
% into the stream.

st_render_string(String, Data, Stream, File, Options):-
    must_be(string, String),
    string_codes(String, Codes),
    merge_defaults(Options, ActualOptions),
    st_render_codes(Codes, Data, Stream, File, ActualOptions).

%! st_render_file(+File, +Data, +Stream, +Options) is det.
%
% Renders given file with the given data
% into the stream.

st_render_file(File, Data, Stream, Options):-
    must_be(ground, File),
    merge_defaults(Options, ActualOptions),
    render_file(File, Data, Stream, ActualOptions).

%! st_render_codes(+Codes, +Data, +Stream, +File) is det.
%
% Renders codes with the given data into the stream.
% File argument is used for resolving includes.

st_render_codes(Codes, Data, Stream, File, Options):-
    must_be(list, Codes),
    must_be(ground, File),
    merge_defaults(Options, ActualOptions),
    st_parse(Codes, Templ, ActualOptions),
    render_scope(Templ, Data, Stream, File, ActualOptions).

render_file(File, Data, Stream, Options):-
    st_resolve(File, AbsFile, Options),
    template(AbsFile, Templ, Options),
    render_scope(Templ, Data, Stream, AbsFile, Options).

% Reads template from file.
% Uses cached template when
% it is available.

template(File, Templ, Options):-
    (   option(cache(true), Options)
    ->  (   st_cached(File, Templ)
        ->  true
        ;   read_file(File, Templ, Options),
            st_cache_put(File, Templ))
    ;   read_file(File, Templ, Options)).

% Actually reads the template file.

read_file(File, Template, Options):-
    option(encoding(Encoding), Options),
    read_file_to_codes(File, Codes, [encoding(Encoding)]),
    st_parse(Codes, Template, Options).

% Renders escaped output.
% Example: {{ title }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = out(Expr), !,
    st_eval(Expr, Scope, Options, Value),
    st_write_escape(Stream, Value),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders unescaped output.
% Example: {{- title }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = out_unescaped(Expr), !,
    st_eval(Expr, Scope, Options, Value),
    write(Stream, Value),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders each loop with element variable.
% Example: {{ each items, item }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = each(Expr, Var, Nested), !,
    st_eval(Expr, Scope, Options, Values),
    (   is_list(Values)
    ->  ((  member(Value, Values),
            put_dict(Var, Scope, Value, NestScope),
            render_scope(Nested, NestScope, Stream, File, Options),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File, Options)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders each loop with element and index variables.
% Example: {{ each items, item, i }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = each(Expr, Var, IVar, Nested), !,
    st_eval(Expr, Scope, Options, Values),
    (   is_list(Values)
    ->  Counter = counter(0),
        ((  member(Value, Values),
            arg(1, Counter, I0),
            I is I0 + 1,
            nb_setarg(1, Counter, I),
            put_dict(Var, Scope, Value, Tmp),
            put_dict(IVar, Tmp, I0, NestScope),
            render_scope(Nested, NestScope, Stream, File, Options),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File, Options)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders each loop with element, index and length
% variables.
% Example: {{ each items, item, i, len }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = each(Expr, Var, IVar, LVar, Nested), !,
    st_eval(Expr, Scope, Options, Values),
    (   is_list(Values)
    ->  length(Values, Length),
        put_dict(LVar, Scope, Length, Tmp1),
        Counter = counter(0),
        ((  member(Value, Values),
            arg(1, Counter, I0),
            I is I0 + 1,
            nb_setarg(1, Counter, I),
            put_dict(Var, Tmp1, Value, Tmp2),
            put_dict(IVar, Tmp2, I0, NestScope),
            render_scope(Nested, NestScope, Stream, File, Options),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File, Options)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders normal text blocks.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = text(Text), !,
    write(Stream, Text),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders include block {{ include path/to/file }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = include(Path), !,
    st_resolve_include(Path, File, AbsFile),
    render_file(AbsFile, Scope, Stream, Options),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders include with specific scope variable.
% Example: {{ include path/to/file, variable }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = include(Path, Var), !,
    st_resolve_include(Path, File, AbsFile),
    st_eval(Var, Scope, Options, Value),
    render_file(AbsFile, Value, Stream, Options),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders dynamic include block.
% Example: {{ dynamic_include file }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = dynamic_include(FileVar), !,
    st_eval(FileVar, Scope, Options, Path),
    st_resolve_include(Path, File, AbsFile),
    render_file(AbsFile, Scope, Stream, Options),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders dynamic include with specific scope variable.
% Example: {{ dynamic_include file variable }}.

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = dynamic_include(FileVar, Var), !,
    st_eval(FileVar, Scope, Options, Path),
    st_resolve_include(Path, File, AbsFile),
    st_eval(Var, Scope, Options, Value),
    render_file(AbsFile, Value, Stream, Options),
    render_scope(Blocks, Scope, Stream, File, Options).

% Renders conditional block.
% Example: {{ if Cond }} a {{ else }} b {{ end }}

render_scope([Block|Blocks], Scope, Stream, File, Options):-
    Block = if(Cond, True, False), !,
    st_eval(Cond, Scope, Options, CondValue),
    (   (CondValue = 0 ; CondValue = false)
    ->  render_scope(False, Scope, Stream, File, Options)
    ;   render_scope(True, Scope, Stream, File, Options)),
    render_scope(Blocks, Scope, Stream, File, Options).

render_scope([Block|_], _, _, _, _):-
    throw(error(unknown_block(Block))).

render_scope([], _, _, _, _).
