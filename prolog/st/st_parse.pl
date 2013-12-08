:- module(st_parse, [
    st_parse/2
]).

:- use_module(st_tokens).

%% st_parse(+Codes, -Templ) is det.
%
% Parses given list of codes into
% a template.
%
% Throws error(unexpected_block_end)
% when block nesting has one extra end.
%
% Throws error(expecting_block_end)
% when block nesting has missing end.

st_parse(Codes, Blocks):-
    st_tokens(Codes, Tokens),
    phrase(blocks(Tmp), Tokens, Rest), !,
    check_rest(Rest),
    Blocks = Tmp.

check_rest([]):- !.

check_rest([end|_]):-
    throw(error(unexpected_block_end)).
    
check_rest([cond_end|_]):-
    throw(error(unexpected_cond_end)).
    
check_rest([cond_else|_]):-
    throw(error(unexpected_cond_else)).

blocks([Block|Blocks]) -->
    block(Block), !,
    blocks(Blocks).
    
blocks([]) --> [].

% FIXME unknown block error.

block(out(Term)) -->
    [out(Term)].
    
block(out_unescaped(Term)) -->
    [out_unescaped(Term)].

block(block(Term, Blocks)) -->
    [block(Term)], blocks(Blocks), block_end.
    
block(cond(Term, TrueBlocks, FalseBlocks)) -->
    [cond_if(Term)], blocks(TrueBlocks), cond_else(FalseBlocks).
    
block(text(Text)) -->
    [text(Text)].
    
block(include(Text)) -->
    [include(Text)].
    
block(include(Text, Var)) -->
    [include(Text, Var)].
    
block(call(Term)) -->
    [call(Term)].
    
block_end -->
    [end], !.
    
block_end -->
    { throw(error(expecting_block_end)) }.

cond_else([]) -->
    [cond_end], !.
    
cond_else(Blocks) -->
    [cond_else], blocks(Blocks), cond_end, !.
    
cond_else(_) -->
    { throw(error(expecting_cond_else_or_cond_end)) }.

cond_end -->
    [cond_end], !.
    
cond_end -->
    { throw(error(expecting_cond_end)) }.
