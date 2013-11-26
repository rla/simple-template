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
    (   Rest = [end|_]
    ->  throw(error(unexpected_block_end))
    ;   Blocks = Tmp).

blocks([Block|Blocks]) -->
    block(Block), !,
    blocks(Blocks).
    
blocks([]) --> [].

block(out(Term)) -->
    [out(Term)].
    
block(out_unescaped(Term)) -->
    [out_unescaped(Term)].

block(block(Term, Blocks)) -->
    [block(Term)], blocks(Blocks), block_end.
    
block(text(Text)) -->
    [text(Text)].
    
block(include(Text)) -->
    [include(Text)].
    
block_end -->
    [end], !.
    
block_end -->
    { throw(error(expecting_block_end)) }.
