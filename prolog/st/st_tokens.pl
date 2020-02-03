:- module(st_tokens, [
    st_tokens/3
]).

/** <module> Template tokenizer

Recognizes tokens from symbol codes.
*/

:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(option)).

%! st_tokens(+Codes, +Options, -Tokens) is det.
%
% Tokenizes the given input into tokens.
%
% Throws error(invalid_input(String)) when
% the input in out/block instruction cannot
% be parsed into a Prolog term.

st_tokens(Codes, Options, Tokens):-
    option(frontend(Frontend), Options),
    (( Frontend =
            syntax_tokens(
                comment(_, _),
                out(_, _),
                out_unescaped(_, _),
                statement(_, _)),
        SyntaxTokens = Frontend
    ) ; ( 
        must_be(oneof([simple, semblance]), Frontend),
        named_syntax_tokens(Frontend, SyntaxTokens)
    )),
    phrase(tokens(Tmp1, SyntaxTokens), Codes),
    phrase(collapse(Tmp2), Tmp1), !,
    Tokens = Tmp2.

named_syntax_tokens(simple, syntax_tokens(
    comment("{{%","}}"),
    out("{{=","}}"),
    out_unescaped("{{-","}}"),
    statement("{{","}}"))).

named_syntax_tokens(semblance, syntax_tokens(
    comment("{#","#}"),
    out("{{","}}"),
    out_unescaped(keyword_unescape_start("{%"),"%}"),
    statement("{%","%}"))).

keyword_unescape_start(StartToken) --> StartToken, whites, "unescape".

tokens(Tokens, Frontend) -->
    comment(Frontend), !,
    tokens(Tokens, Frontend).

tokens([Token|Tokens], Frontend) -->
    token(Token, Frontend), !,
    tokens(Tokens, Frontend).

tokens([], _) --> "".

comment(syntax_tokens(comment(Start, End), out(_,_), out_unescaped(_,_), statement(_,_))) -->
    Start, string(_), End, !.

out(syntax_tokens(comment(_,_), out(Start, End), out_unescaped(_,_), statement(_,_)), out(Term)) -->
    { string_codes(End, EndCodes) },
    Start, whites, term_to_token(Term, EndCodes), !.

out_unescaped(syntax_tokens(comment(_,_), out(_,_), out_unescaped(Start, End), statement(_,_)), out_unescaped(Term)) -->
    { string_codes(End, EndCodes) },
    Start, whites, term_to_token(Term, EndCodes), !.

token(Term, Frontend) -->
    out_unescaped(Frontend, Term), !.

token(Term, Frontend) -->
    out(Frontend, Term), !.

token(end, Frontend) -->
    start(Frontend), whites, "end", whites, end(Frontend), !.

token(else, Frontend) -->
    start(Frontend), whites, "else", whites, end(Frontend), !.

token(Token, Frontend) -->
    start(Frontend), whites, "include ", whites, term(Term, Frontend), !,
    { token_term_include(Term, Token) }.

token(Token, Frontend) -->
    start(Frontend), whites, "dynamic_include ", whites, term(Term, Frontend), !,
    { token_term_dyn_include(Term, Token) }.

token(Token, Frontend) -->
    start(Frontend), whites, "block", whites, term(Term, Frontend), !,
    { token_term_block(Term, Token) }.

token(slot, Frontend) -->
    start(Frontend), whites, "slot", whites, end(Frontend), !.

token(if(Cond), Frontend) -->
    start(Frontend), whites, "if ", whites, term(Cond, Frontend), !.

token(else_if(Cond), Frontend) -->
    start(Frontend), whites, "else ", whites, "if ", whites, term(Cond, Frontend), !.

token(Token, Frontend) -->
    start(Frontend), whites, "each ", whites, term(Term, Frontend), !,
    { token_term_each(Term, Token) }.

token(_, Frontend) -->
    invalid(Frontend).

token(Code, _) -->
    [Code].

start(syntax_tokens(comment(_,_), out(_,_), out_unescaped(_,_), statement(Start, _))) -->
    Start, !.

end(syntax_tokens(comment(_,_), out(_,_), out_unescaped(_,_), statement(_, End))) -->
    End, !.

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

% Turns term into an include token.

% FIXME validate path spec.

token_term_include(Term, Token):-
    (   Term =.. [',', File, Var]
    ->  Token = include(File, Var)
    ;   Token = include(Term)).

% Turns term into a dynamic include token.

token_term_dyn_include(Term, Token):-
    (   Term = ','(File, Var)
    ->  Token = dynamic_include(File, Var)
    ;   Token = dynamic_include(Term)).

% Turns term into an each token.

token_term_each(Term, Token):-
    (   Term = ','(Items, ','(Item, ','(Index, Len)))
        ->  Token = each(Items, Item, Index, Len)
        ;   (   Term = ','(Items, ','(Item, Index))
            ->  Token = each(Items, Item, Index)
            ;   (   Term = ','(Items, Item)
                ->  Token = each(Items, Item)
                ;   throw(error(invalid_each(Term)))))).

% Turns term into a block token.

token_term_block(Term, Token):-
    (   Term =.. [',', File, Var]
    ->  Token = block(File, Var)
    ;   Token = block(Term)).

% Helper to report invalid instructions.

invalid(Frontend) -->
    start(Frontend), whites, [C1,C2,C3,C4,C5],
    {
        atom_codes(Atom, [C1,C2,C3,C4,C5]),
        atom_concat(Atom, '...', At),
        throw(error(invalid_instruction(At)))
    }.

% Extracts term from input.

term(Term, syntax_tokens(comment(_,_), out(_,_), out_unescaped(_,_), statement(_, End))) -->
    { string_codes(End, EndCodes) },
    term_to_token(Term, EndCodes), !.

term_to_token(Term, Delimiter) -->
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
% given delimiter.

codes_delimiter(Delimiter, []) -->
    match_codes(Delimiter), !.

codes_delimiter(Until, [Code|Codes]) -->
    [Code], codes_delimiter(Until, Codes).

match_codes([Code|Codes]) --> [Code], match_codes(Codes).
match_codes([]) --> "".
