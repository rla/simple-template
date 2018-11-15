:- module(st_common_tokens, [
    st_collapse//1,
    st_term//2,
    st_invalid//1,
    st_token_term_include/2,
    st_token_term_dyn_include/2,
    st_token_term_each/2,
    st_token_term_block/2
]).

/** <module> Template tokenizer

Recognizes tokens from symbol codes.
*/

:- use_module(library(dcg/basics)).

% Collapses codes into text tokens.

st_collapse([Token|Tokens]) -->
    text(Token), !,
    st_collapse(Tokens).

st_collapse([Token|Tokens]) -->
    [Token], st_collapse(Tokens).

st_collapse([]) --> [].

text(text(Codes)) -->
    text_codes(Codes).

text_codes([Code|Codes]) -->
    text_code(Code),
    text_codes(Codes).

text_codes([Code]) -->
    text_code(Code).

text_code(Code) -->
    [Code], { number(Code) }.

% Turns term into an include token.

% FIXME validate path spec.

st_token_term_include(Term, Token):-
    (   Term =.. [',', File, Var]
    ->  Token = include(File, Var)
    ;   Token = include(Term)).

% Turns term into a dynamic include token.

st_token_term_dyn_include(Term, Token):-
    (   Term = ','(File, Var)
    ->  Token = dynamic_include(File, Var)
    ;   Token = dynamic_include(Term)).

% Turns term into an each token.

st_token_term_each(Term, Token):-
    (   Term = ','(Items, ','(Item, ','(Index, Len)))
        ->  Token = each(Items, Item, Index, Len)
        ;   (   Term = ','(Items, ','(Item, Index))
            ->  Token = each(Items, Item, Index)
            ;   (   Term = ','(Items, Item)
                ->  Token = each(Items, Item)
                ;   throw(error(invalid_each(Term)))))).

% Turns term into a block token.

st_token_term_block(Term, Token):-
    (   Term =.. [',', File, Var]
    ->  Token = block(File, Var)
    ;   Token = block(Term)).

% Helper to report invalid instructions.
% Begin is either {{ or {%.

st_invalid(Begin) -->
    begin_match(Begin), whites, [C1,C2,C3,C4,C5],
    {
        atom_codes(Atom, [C1,C2,C3,C4,C5]),
        atom_concat(Atom, '...', At),
        throw(error(invalid_instruction(At)))
    }.

begin_match("{{") -->
    "{{".

begin_match("{%") -->
    "{%".

% Extracts term from input. Term is delimited
% with the given delimiter (either }} or %}).

st_term(Term, Delimiter) -->
    codes_delimiter(Delimiter, Codes),
    {
        (   read_term_from_codes(Codes, Term, [])
        ->  (   ground(Term)
            ->  true
            ;   throw(error(non_ground_expression(Term))))
        ;   string_codes(String, Codes),
            throw(error(invalid_input(String))))
    }.

% Takes input up to and including the
% given delimiter (either }} or %}).

codes_delimiter("}}", []) -->
    "}}", !.

codes_delimiter("%}", []) -->
    "%}", !.

codes_delimiter(Until, [Code|Codes]) -->
    [Code], codes_delimiter(Until, Codes).
