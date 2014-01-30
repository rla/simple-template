:- module(st_white, [
    st_strip_indent/2
]).

/** <module> Whitespace handling

Removes indents and collapses extra line ends.
*/

:- use_module(library(dcg/basics)).

%! st_strip_indent(+Codes, -StrippedCodes) is det.
%
% Removes indents and collapses extra line ends.

st_strip_indent(Codes, Stripped):-
    phrase(strip_rest(Tmp), Codes),
    phrase(collapse(Out), Tmp), !,
    Stripped = Out.

strip_rest([0'\n|Codes]) -->
    whites, ln, whites, !,
    strip_rest(Codes).

strip_rest([]) -->
    eos, !.

strip_rest([Code|Codes]) -->
    [Code], strip_rest(Codes).

% Collapses duplicate line ends.
% Assumes that line ends are normalized
% to \n.

collapse([0'\n|Codes]) -->
    line_ends, !, collapse(Codes).

collapse([Code|Codes]) -->
    [Code], !, collapse(Codes).

collapse([]) --> "".

line_ends -->
    "\n", line_ends.

line_ends -->
    "\n".

ln --> "\r\n", !.
ln --> "\n", !.
ln --> "\r".
