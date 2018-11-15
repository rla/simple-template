:- module(st_semblance_tokens, [
    st_semblance_tokens/2
]).

/** <module> Template tokenizer

Recognizes tokens from symbol codes.

This is an alternate tokenizer which uses the semblance type
tag format (which aims to be more like django/djula/twig tag types).
*/

:- use_module(library(dcg/basics)).
:- use_module(st_common_tokens).

%! semblance_tokens(+Codes, -Tokens) is det.
%
% Tokenizes the given input into tokens.
%
% Throws error(invalid_input(String)) when
% the input in out/block instruction cannot
% be parsed into a Prolog term.

st_semblance_tokens(Codes, Tokens):-
    phrase(tokens(Tmp1), Codes),
    phrase(st_collapse(Tmp2), Tmp1), !,
    Tokens = Tmp2.

tokens(Tokens) -->
    comment, !,
    tokens(Tokens).

tokens([Token|Tokens]) -->
    token(Token), !,
    tokens(Tokens).

tokens([]) --> "".

comment -->
    "{#", string(_), "#}".

token(out(Term)) -->
    "{{", whites, st_term(Term, "}}"), !.

token(out_unescaped(Term)) -->
    "{%", whites, "unescape", whites, st_term(Term, "%}"), !.

token(end) -->
    "{%", whites, "end", whites, "%}", !.

token(else) -->
    "{%", whites, "else", whites, "%}", !.

token(Token) -->
    "{%", whites, "include ", whites, st_term(Term, "%}"), !,
    { st_token_term_include(Term, Token) }.

token(Token) -->
    "{%", whites, "dynamic_include ", whites, st_term(Term, "%}"), !,
    { st_token_term_dyn_include(Term, Token) }.

token(Token) -->
    "{%", whites, "block", whites, st_term(Term, "%}"), !,
    { st_token_term_block(Term, Token) }.

token(slot) -->
    "{%", whites, "slot", whites, "%}".

token(if(Cond)) -->
    "{%", whites, "if ", whites, st_term(Cond, "%}"), !.

token(else_if(Cond)) -->
    "{%", whites, "else ", whites, "if ", whites, st_term(Cond, "%}"), !.

token(Token) -->
    "{%", whites, "each ", whites, st_term(Term, "%}"), !,
    { st_token_term_each(Term, Token) }.

token(_) -->
    st_invalid("{%").

token(Code) -->
    [Code].
