:- module(st_tokens, [
    st_tokens/2
]).

/** <module> Template tokenizer

Recognizes tokens from symbol codes.
*/

:- use_module(library(dcg/basics)).

%! st_tokens(+Codes, -Tokens) is det.
%
% Tokenizes the given input into tokens.
%
% Throws error(invalid_input(String)) when
% the input in out/block instruction cannot
% be parsed into a Prolog term.

st_tokens(Codes, Tokens):-
    phrase(tokens(Tmp1), Codes),
    phrase(collapse(Tmp2), Tmp1), !,
    Tokens = Tmp2.

% Collapses codes into text tokens.
    
collapse([Token|Tokens]) -->
    text(Token), !,
    collapse(Tokens).
    
collapse([Token|Tokens]) -->
    [Token], collapse(Tokens).
    
collapse([]) --> [].

text(text(Codes)) -->
    text_codes(Codes).

text_codes([Code|Codes]) -->
    text_code(Code),
    text_codes(Codes).
    
text_codes([Code]) -->
    text_code(Code).
    
text_code(Code) -->
    [Code], { number(Code) }.

tokens([Token|Tokens]) -->
    token(Token),
    tokens(Tokens).

tokens([]) --> "".

token(out(Term)) -->
    "{{=", whites, term(Term), !.
    
token(out_unescaped(Term)) -->
    "{{-", whites, term(Term), !.

token(end) -->
    "{{", whites, "end", whites, "}}", !.

token(else) -->
    "{{", whites, "else", whites, "}}", !.

% FIXME validate path spec.

token(Token) -->
    "{{", whites, "include ", whites, term(Term), !,
    {
        (   Term =.. [',', File, Var]
        ->  Token = include(File, Var)
        ;   Token = include(Term))
    }.

token(Token) -->
    "{{", whites, "dynamic_include ", whites, term(Term), !,
    {
        (   Term = ','(File, Var)
        ->  Token = dynamic_include(File, Var)
        ;   Token = dynamic_include(Term))
    }.

token(if(Cond)) -->
    "{{", whites, "if ", whites, term(Cond), !.

token(else_if(Cond)) -->
    "{{", whites, "else ", whites, "if ", whites, term(Cond), !.

token(Token) -->
    "{{", whites, "each ", whites, term(Term), !,
    {
        (   Term = ','(Items, ','(Item, ','(Index, Len)))
        ->  Token = each(Items, Item, Index, Len)
        ;   (   Term = ','(Items, ','(Item, Index))
            ->  Token = each(Items, Item, Index)
            ;   (   Term = ','(Items, Item)
                ->  Token = each(Items, Item)
                ;   throw(error(invalid_each(Term))))))
    }.

token(Code) -->
    [Code].

term(Term) -->
    term_codes(Codes),
    {
        (   read_term_from_codes(Codes, Term, [])
        ->  true
        ;   string_codes(String, Codes),
            throw(error(invalid_input(String))))
    }.

term_codes([]) --> "}}", !.

term_codes([Code|Codes]) -->
    [Code], term_codes(Codes).
