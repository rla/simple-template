:- module(st_common_tokens, [
    collapse/3,
    text/3,
    text_codes/3,
    text_code/3
]).

/** <module> Template tokenizer

Recognizes tokens from symbol codes.
*/

:- use_module(library(dcg/basics)).

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
