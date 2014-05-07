:- module(st_render, [
    st_render_string/3, % +String, +Data, +File
    st_render_string/4, % +String, +Data, +Stream, +File
    st_render_file/2,   % +File, +Data
    st_render_file/3,   % +File, +Data, +Stream
    st_render_codes/3,  % +Codes, +Data, +File
    st_render_codes/4,  % +Codes, +Data, +Stream, +File
    st_set_encoding/1   % +Atom
]).

/** <module> Template renderer

Turns template together with data into output.
*/

:- use_module(library(readutil)).
:- use_module(library(error)).
:- use_module(library(debug)).

:- use_module(st_parse).
:- use_module(st_expr).
:- use_module(st_file).
:- use_module(st_escape).
:- use_module(st_funs).

%! st_render_string(+String, +Data, +File) is det.
%
% Renders given string with the given data.
% Calls st_render_string/4 with the current stream.
% File name is added current extension.

st_render_string(String, Data, File):-
    current_output(Stream),
    st_render_string(String, Data, Stream, File).

%! st_render_string(+String, +Data, +Stream, +File) is det.
%
% Renders given string with the given data
% into the stream.

st_render_string(String, Data, Stream, File):-
    string_codes(String, Codes),
    st_render_codes(Codes, Data, Stream, File).

%! st_render_file(+File, +Data) is det.
%
% Renders given file with the given data.
% Calls st_render_file/3 with the current stream.
% File name is added current extension.
    
st_render_file(File, Data):-
    current_output(Stream),
    st_render_file(File, Data, Stream).

%! st_render_file(+File, +Data, +Stream) is det.
%
% Renders given file with the given data
% into the stream.
    
st_render_file(File, Data, Stream):-
    render_file(File, Data, Stream).

%! st_render_codes(+Codes, +Data, +File) is det.
%
% Renders codes with the given data.
% Calls st_render_codes/3 with the current stream.
    
st_render_codes(Codes, Data, File):-
    current_output(Stream),
    st_render_codes(Codes, Data, Stream, File).

%! st_render_codes(+Codes, +Data, +Stream, +File) is det.
%
% Renders codes with the given data into the stream.
% File argument is used for resolving includes.
    
st_render_codes(Codes, Data, Stream, File):-
    st_parse(Codes, Templ),
    render_scope(Templ, Data, Stream, File).
    
render_file(File, Data, Stream):-
    st_resolve(File, AbsFile),
    template(AbsFile, Templ),
    render_scope(Templ, Data, Stream, AbsFile).

:- dynamic(st_encoding/1).

%! st_set_encoding(+Encoding) is det.
%
% Sets the encoding used for opening
% template files. Allowed values are
% same as encoding option values for
% read_file_to_codes/3. When no encoding
% is set, default behavior is used. No
% encoding option is then passed to
% read_file_to_codes/3.

st_set_encoding(Encoding):-
    retractall(st_encoding(_)),
    assertz(st_encoding(Encoding)).

% Reads template from file.
% Uses cached template when
% it is available.

template(File, Templ):-
    (   st_cached(File, Templ)
    ->  true
    ;   (   st_encoding(Encoding)
        ->  Options = [encoding(Encoding)]
        ;   Options = []),
        read_file_to_codes(File, Codes, Options),
        st_parse(Codes, Templ),
        st_cache_put(File, Templ)).

% Renders escaped output.
% Example: {{= title }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = out(Expr), !,
    st_eval(Expr, Scope, Value),
    st_write_escape(Stream, Value),
    render_scope(Blocks, Scope, Stream, File).

% Renders unescaped output.
% Example: {{- title }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = out_unescaped(Expr), !,
    st_eval(Expr, Scope, Value),
    write(Stream, Value),
    render_scope(Blocks, Scope, Stream, File).

% Renders each loop with element variable.
% Example: {{ each items, item }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = each(Expr, Var, Nested), !,
    st_eval(Expr, Scope, Values),
    (   is_list(Values)
    ->  ((  member(Value, Values),
            put_dict(Var, Scope, Value, NestScope),
            render_scope(Nested, NestScope, Stream, File),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders each loop with element and index variables.
% Example: {{ each items, item, i }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = each(Expr, Var, IVar, Nested), !,
    st_eval(Expr, Scope, Values),
    (   is_list(Values)
    ->  Counter = counter(0),
        ((  member(Value, Values),
            arg(1, Counter, I0),
            I is I0 + 1,
            nb_setarg(1, Counter, I),
            put_dict(Var, Scope, Value, Tmp),
            put_dict(IVar, Tmp, I0, NestScope),
            render_scope(Nested, NestScope, Stream, File),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders each loop with element, index and length
% variables.
% Example: {{ each items, item, i, len }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = each(Expr, Var, IVar, LVar, Nested), !,
    st_eval(Expr, Scope, Values),
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
            render_scope(Nested, NestScope, Stream, File),
            fail) ; true),
        render_scope(Blocks, Scope, Stream, File)
    ;   throw(error(expr_in_each_not_list(Expr)))).

% Renders normal text blocks.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = text(Text), !,
    write(Stream, Text),
    render_scope(Blocks, Scope, Stream, File).

% Renders include block {{ include path/to/file }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = include(Path), !,
    st_resolve_include(Path, File, AbsFile),
    render_file(AbsFile, Scope, Stream),
    render_scope(Blocks, Scope, Stream, File).

% Renders include with specific scope variable.
% Example: {{ include path/to/file, variable }}.
    
render_scope([Block|Blocks], Scope, Stream, File):-
    Block = include(Path, Var), !,
    st_resolve_include(Path, File, AbsFile),
    st_eval(Var, Scope, Value),
    render_file(AbsFile, Value, Stream),
    render_scope(Blocks, Scope, Stream, File).

% Renders dynamic include block.
% Example: {{ dynamic_include file }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = dynamic_include(FileVar), !,
    st_eval(FileVar, Scope, Path),
    st_resolve_include(Path, File, AbsFile),
    render_file(AbsFile, Scope, Stream),
    render_scope(Blocks, Scope, Stream, File).

% Renders dynamic include with specific scope variable.
% Example: {{ dynamic_include file variable }}.

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = dynamic_include(FileVar, Var), !,
    st_eval(FileVar, Scope, Path),
    st_resolve_include(Path, File, AbsFile),
    st_eval(Var, Scope, Value),
    render_file(AbsFile, Value, Stream),
    render_scope(Blocks, Scope, Stream, File).

% Renders conditional block.
% Example: {{ if Cond }} a {{ else }} b {{ end }}

render_scope([Block|Blocks], Scope, Stream, File):-
    Block = if(Cond, True, False), !,
    st_eval(Cond, Scope, CondValue),
    (   (CondValue = 0 ; CondValue = false)
    ->  render_scope(False, Scope, Stream, File)
    ;   render_scope(True, Scope, Stream, File)),
    render_scope(Blocks, Scope, Stream, File).

render_scope([Block|_], _, _, _):-
    throw(error(unknown_block(Block))).

render_scope([], _, _, _).
