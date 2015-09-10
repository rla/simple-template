# simple-template

Text (HTML) template processor for Swi-Prolog. Works
best for cases when you have mainly static HTML.

[![Build Status](https://travis-ci.org/rla/simple-template.svg)](https://travis-ci.org/rla/simple-template)

## Example

Input markup (`test.html` file):

    <h1>{{= title }}</h1>
    {{ each items, item }}
        <h2>{{= item.title }}</h2>
        <div class="content">{{- item.content }}</div>
    {{ end }}

rendering with data:

    :- use_module(library(st/st_file)).
    :- use_module(library(st/st_render)).

    :- st_set_extension(html).

    st_render_file(test, _{
        title: 'Hello',
        items: [
            _{ title: 'Item 1', content: 'Abc 1' },
            _{ title: 'Item 1', content: 'Abc 2' }
        ]
    }).

output:

    <h1>Hello</h1>

    <h2>Item 1</h2>
    <div class="content">Abc 1</div>

    <h2>Item 1</h2>
    <div class="content">Abc 2</div>

## Processing instructions

Processing instructions start with double opening curly braces (`{{`) and
end with double closing curly braces (`}}`).

There are 4 types of processing instructions.

### Output

There are two output instructions. `{{= expression }}` outputs the expression
value and escapes the special HTML characters. `{{- expression }}` outputs the
expression value but does not escape the output. There must be no space
between `{{` and `=` or `-`.

### Includes

Includes use the syntax `{{ include path/to/file }}`. The path should be relative.
The relative path is resolved against the current file location and the currently
set file extension is added. All values from the current scope are available to
the included file. There is also `{{ include path/to/file, expression }}` that
sets the scope of the included file to the value of the expression (must be a dict).

Dynamic includes use the syntax `{{ dynamic_include expression }}`. The value of
the expression must be a file path specifier. There is also a variant with the
scope expression.

Includes are currently processed at runtime. You should enable caching if you include
inside a loop otherwise the included file is parsed over and over again.

### Conditionals

Conditional instructions have the following syntax:

    {{ if cond_expression1 }}
    ...
    {{ else if cond_expression2 }}
    ...
    {{ else }}
    ...
    {{ end }}

`else if` and `else` parts are optional.

### Each loop

The each loop allows to process lists. The syntax for each loop:

    {{ each expression, item_var, index_var, len_var }}
    ...
    {{ end }}

The value of expression must be a list. `item_var`, `index_var` and `len_var`
refer to current item, current item index and the length of the list.
`index_var` and `len_var` are optional.

## Expressions

Expressions are ground Prolog terms with the interpretation
described below.

In most contexts, atoms are interpreted as scope entries ("variables").
An error is thrown when there is no suitable entry.

The following arithmetic operators and functions are supported:
`-, +, *, /, mod, rem, //, div, abs, sign, max, min, random, round, truncate, floor, ceiling, **, ^`.

Boolean expressions include modified unification using the `=` operator. The
operator provides coercion from atom to string when either of operands is an atom
and the other is a string. For other types, no coercion is applied. Similarily works
the `\=` operator. Supported boolean operators are: `,` (logical and), `;` (logical or)
and `\+` (logical negation). Supported comparison operator are: `>, <, >=, =<,`.

Conditional expression `if(expression, true_expr, false_expr)` value is the value of
`true_expr` when the `expression` evaluates to anything but 0 or `false`. Otherwise the
value is the value of `false_expr`.

The `+` operator is overloaded to provide string or atom concatenation when
the left side of the operator is an atom or string. The result is string.

The `.` operator is used for accessing values from dicts. Internally the
`./3` predicate is used which means that user-defined functions on dicts
will work as intended.

Compound terms that match none of the operators and constructs described above are
treated as user-defined (see below) function calls. An error is thrown when there is no such function
defined.

A special expression is `atom(something)`. Its value is atom `something`. This
is added to differentiate them from other atoms which are otherwise interpreted
as scope entries.

Literal lists work as expected with the elements having the interpretation as
the expressions above.

## Built-in functions

There are three single-argument functions to encode URI parts: `encode_path`,
`encode_query_value` and `encode_fragment`. These call
[uri_encoded/3](http://swi-prolog.org/pldoc/doc_for?object=uri_encoded/3) with
the respective first argument.

## Global constants

Global constants can be set with the `st_set_global/2` predicate by importing
the `st_expr` module. When a scope contains an
entry with the same name as a global then the local value is preferred.

## User-defined functions

User-defined functions can be set with the `st_set_function(Name, Arity, Goal)`
predicate in the `st_expr` module. Arity refers to the function arity. Goal must
have arity Arity + 1.

## Comments

Comment blocks start with `{{%` and end with `}}`. Comments do not appear
in the output.

## Caching

Template caching is enabled by importing the `st_file` module and calling
the `st_enable_cache` predicate. This makes the system cache parsed templates.
This is particulary useful when using includes in loops. To purge the current
cache contents, use the `st_cache_invalidate` predicate.

## Whitespace removal

Some (but not all) whitespace is removed by calling the `st_enable_strip_white`
predicate from the `st_parse` module. Line indents and some duplicate line ends
are removed from the output. Whitespace removal is parse-time and does not
incur any runtime penalty.

## Encoding

Encoding for template files can be specified with the `st_set_encoding/1` predicate
in the `st_render` module. Accepted values are all that are accepted by the `encoding`
option of the `read_file_to_codes/3` predicate.

## Installation

This package requires Swi-Prolog 7.x.

    pack_install('http://packs.rlaanemets.com/simple-template/simple_template-*.tgz').

## API documentation

See <http://packs.rlaanemets.com/simple-template/doc/>.

## Running tests

In the package root, insert into swipl:

    [tests/tests].
    run_tests.

Or if you cloned the repo:

    make test

## Bug reports/feature requests

Please send bug reports/feature request through the GitHub
project [page](https://github.com/rla/simple-template).

## Changelog

 * 2014-05-09 version 0.3.0. Provide st_cache_invalidate/0, \= operator, comments.
 * 2014-05-07 version 0.2.0. Literal lists, encode_* functions.
 * 2014-03-02 version 0.1.0. Provided st_set_encoding/1.
 * 2014-01-30 version 0.0.1

## License

The MIT License. See LICENSE file.
