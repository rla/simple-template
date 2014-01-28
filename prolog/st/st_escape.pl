:- module(st_escape, [
    st_write_escape/2 % +Stream, +Text
]).

%! st_write_escape(+Stream, +Text) is det.
%
% Writes text into Stream by escaping some
% of the symbols that cause problem in HTML.

st_write_escape(Stream, Text):-
    string(Text), !,
    string_codes(Text, Codes),
    st_write_codes(Codes, Stream).

st_write_escape(Stream, Text):-
    atom(Text), !,
    atom_codes(Text, Codes),
    st_write_codes(Codes, Stream).

st_write_escape(Stream, Text):-
    number(Text), !,
    write(Stream, Text).

% Convert other terms into strings
% and output them.

st_write_escape(Stream, Text):-
    term_string(Text, String),
    string_codes(String, Codes),
    st_write_codes(Stream, Codes).

st_write_codes([0'<|Codes], Stream):- !,
    write(Stream, '&lt;'),
    st_write_codes(Codes, Stream).

st_write_codes([0'>|Codes], Stream):- !,
    write(Stream, '&gt;'),
    st_write_codes(Codes, Stream).

st_write_codes([0'&|Codes], Stream):- !,
    write(Stream, '&amp;'),
    st_write_codes(Codes, Stream).

st_write_codes([0'\'|Codes], Stream):- !,
    write(Stream, '&#39;'),
    st_write_codes(Codes, Stream).

st_write_codes([0'"|Codes], Stream):- !,
    write(Stream, '&quot;'),
    st_write_codes(Codes, Stream).

st_write_codes([Code|Codes], Stream):- !,
    put_code(Stream, Code),
    st_write_codes(Codes, Stream).

st_write_codes([], _).
