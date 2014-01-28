:- module(st_parse, [
    st_parse/2, % +Codes, -Blocks
    st_enable_strip_white/0,
    st_disable_strip_white/0
]).

:- use_module(st_tokens).
:- use_module(st_white).

:- dynamic(strip_white).

%! st_enable_strip_white is det.
%
% Enables whitespace stripping.

st_enable_strip_white:-
    (   strip_white
    ->  true
    ;   assertz(strip_white)).

%! st_disable_strip_white is det.
%
% Disables whitespace stripping.

st_disable_strip_white:-
    retractall(strip_white).

%! st_parse(+Codes, -Templ) is det.
%
% Parses given list of codes into
% a template.
%
% Throws various parsing errors.

st_parse(Codes, Blocks):-
    st_tokens(Codes, Tokens),
    phrase(blocks(Tmp), Tokens, Rest), !,
    check_rest(Rest),
    Blocks = Tmp.

% Checks the remaining tokens.
% Some tokens (end, else, else_if)
% could appear by mistake without the
% block-starting token. This will catch
% such errors.

check_rest([]):- !.

check_rest([end|_]):-
    throw(error(unexpected_block_end)).
    
check_rest([else|_]):-
    throw(error(unexpected_else)).

check_rest([else_if(_)|_]):-
    throw(error(unexpected_else_if)).

% Takes as many blocks as possible.

blocks([Block|Blocks]) -->
    block(Block), !,
    blocks(Blocks).
    
blocks([]) --> [].

% Output statement.

block(out(Term)) -->
    [out(Term)].
    
% Unescaped output statement.

block(out_unescaped(Term)) -->
    [out_unescaped(Term)].

% Each loop.

block(each(Items, Item, Blocks)) -->
    [each(Items, Item)], blocks(Blocks), block_end.

block(each(Items, Item, Index, Blocks)) -->
    [each(Items, Item, Index)], blocks(Blocks), block_end.

block(each(Items, Item, Index, Len, Blocks)) -->
    [each(Items, Item, Index, Len)], blocks(Blocks), block_end.

% if/else/else if blocks.

block(if(Cond, True, Rest)) -->
    [if(Cond)], blocks(True), cond_rest(Rest).
    
% Text output.

block(text(String)) -->
    [text(Codes)], !,
    {
        (   strip_white
        ->  st_strip_indent(Codes, Stripped),
            string_codes(String, Stripped)
        ;   string_codes(String, Codes))

    }.

% Include.

block(include(File)) -->
    [include(File)].

block(include(File, Var)) -->
    [include(File, Var)].

% Dynamic include.

block(dynamic_include(FileVar)) -->
    [dynamic_include(FileVar)].

block(dynamic_include(FileVar, Var)) -->
    [dynamic_include(FileVar, Var)].

% Recognizes block end or
% throws an error when one not found.

block_end -->
    [end], !.
    
block_end -->
    { throw(error(expecting_block_end)) }.

% Remainer part of if/else if block.

cond_rest([]) -->
    [end], !.
    
cond_rest(Blocks) -->
    [else], blocks(Blocks), block_end, !.

cond_rest([if(Cond, True, Rest)]) -->
    [else_if(Cond)], blocks(True), cond_rest(Rest), !.

cond_rest(_) -->
    { throw(error(expecting_cond_else_or_end)) }.
