:- module(st_tokens, [
    st_tokens/2
]).

%% st_tokens(+Codes, -Tokens) is det.
%
% Tokenizes the given input into tokens.
% Tokens are:
% any atom - verbatim text;
% out(Term) - output instruction (escaped);
% out_unescaped(Term) - output instruction (unescaped);
% block(Term) - block start;
% end - block end;
% include(location) - includes a file;
% cond_if(Term) - conditional block start;
% cond_else - start of conditional else;
% cond_end - conditional block end;
% call(Term) - function call.
%
% Throws error(invalid_input(Atom)) when
% the atom in out/block instruction cannot
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

text(text(Text)) -->
    text_codes(Codes),
    { atom_codes(Text, Codes) }.
    
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

tokens([]) -->
    "".

token(out(Term)) -->
    "[[=", ws, term(Term), !.
    
token(out_unescaped(Term)) -->
    "[[-", ws, term(Term), !.
    
token(Term) -->
    "[[*", ws, term(Term), !.

token(end) -->
    "[[:]]", !.
    
token(block(Term)) -->
    "[[:", ws, term(Term), !.
    
token(cond_end) -->
    "[[?]]", !.
    
token(cond_else) -->
    "[[?", ws, "else", ws, "]]", !.
    
token(cond_if(Term)) -->
    "[[?", ws, term(Term), !.
    
token(call(Term)) -->
    "[[\\", ws, term(Term), !.

token(Code) -->
    [Code].
    
term(Term) -->
    term_codes(Codes),
    {
        atom_codes(Atom, Codes),
        (   read_term_from_atom(Atom, Term, [])
        ->  true
        ;   throw(error(invalid_input(Atom))))
    }.
    
term_codes([]) --> "]]", !.

term_codes([Code|Codes]) -->
    [Code], term_codes(Codes).

ws --> " ", !, ws.
    
ws --> "".
