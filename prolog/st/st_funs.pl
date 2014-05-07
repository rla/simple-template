:- module(st_funs, [
    encode_path/2,        % +Path, -Encoded
    encode_query_value/2, % +QueryValue, -Encoded
    encode_fragment/2     % +Fragment, -Encoded
]).

/** <module> Predefined functions

Defines various helper functions to work with templates,
HTML and otherwise.
*/

:- use_module(library(uri)).
:- use_module(st_expr).

% Function to encode URL paths.

:- st_set_function(encode_path, 1, encode_path).

%! encode_path(+Path, -Encoded) is det.
%
% Implements function to encode the path
% value of an URI. See uri_encoded/3.

encode_path(Path, Encoded):-
    uri_encoded(path, Path, Encoded).

% Function to encode URL query.

:- st_set_function(encode_query_value, 1, encode_query_value).

%! encode_query_value(+QueryValue, -Encoded) is det.
%
% Implements function to encode the query
% value of an URI. See uri_encoded/3.

encode_query_value(Value, Encoded):-
    uri_encoded(query_value, Value, Encoded).

% Function to encode URL fragment.

:- st_set_function(encode_fragment, 1, encode_fragment).

%! encode_fragment(+Fragment, -Encoded) is det.
%
% Implements function to encode the fragment
% of an URI. See uri_encoded/3.

encode_fragment(Fragment, Encoded):-
    uri_encoded(fragment, Fragment, Encoded).
