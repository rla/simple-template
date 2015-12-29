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
    phrase(collapse(Tmp2), Tmp1), !,
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
    "{{", whites, term_bb(Term), !.

token(out_unescaped(Term)) -->
    "{%", whites, "unescape", whites, term(Term), !.

token(end) -->
    "{%", whites, "end", whites, "%}", !.

token(else) -->
    "{%", whites, "else", whites, "%}", !.

% FIXME validate path spec.

token(Token) -->
    "{%", whites, "include ", whites, term(Term), !,
    {
        (   Term =.. [',', File, Var]
        ->  Token = include(File, Var)
        ;   Token = include(Term))
    }.

token(Token) -->
    "{%", whites, "dynamic_include ", whites, term(Term), !,
    {
        (   Term = ','(File, Var)
        ->  Token = dynamic_include(File, Var)
        ;   Token = dynamic_include(Term))
    }.

token(if(Cond)) -->
    "{%", whites, "if ", whites, term(Cond), !.

token(else_if(Cond)) -->
    "{%", whites, "else ", whites, "if ", whites, term(Cond), !.

token(Token) -->
    "{%", whites, "each ", whites, term(Term), !,
    {
        (   Term = ','(Items, ','(Item, ','(Index, Len)))
        ->  Token = each(Items, Item, Index, Len)
        ;   (   Term = ','(Items, ','(Item, Index))
            ->  Token = each(Items, Item, Index)
            ;   (   Term = ','(Items, Item)
                ->  Token = each(Items, Item)
                ;   throw(error(invalid_each(Term))))))
    }.

token(_) -->
    "{%", whites, [C1,C2,C3,C4,C5],
    {
        atom_codes(Atom, [C1,C2,C3,C4,C5]),
        atom_concat(Atom, '...', At),
        throw(error(invalid_instruction(At)))
    }.

token(Code) -->
    [Code].

term(Term) -->
    term_codes(Codes),
    {
        (   read_term_from_codes(Codes, Term, [])
        ->  (   ground(Term)
            ->  true
            ;   throw(error(non_ground_expression(Term))))
        ;   string_codes(String, Codes),
            throw(error(invalid_input(String))))
    }.

term_codes([]) --> "%}", !.

term_codes([Code|Codes]) -->
    [Code], term_codes(Codes).

term_bb(Term) -->
    term_bb_codes(Codes),
    {
        (   read_term_from_codes(Codes, Term, [])
        ->  (   ground(Term)
            ->  true
            ;   throw(error(non_ground_expression(Term))))
        ;   string_codes(String, Codes),
            throw(error(invalid_input(String))))
    }.

term_bb_codes([]) --> "}}", !.

term_bb_codes([Code|Codes]) -->
    [Code], term_bb_codes(Codes).
