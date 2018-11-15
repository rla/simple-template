# simple-template

Text (HTML) template processor for SWI-Prolog. Works
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

    use_module(library(st/st_render)).

    current_output(Out),
    st_render_file(test, _{
        title: 'Hello',
        items: [
            _{ title: 'Item 1', content: 'Abc 1' },
            _{ title: 'Item 1', content: 'Abc 2' }
        ]
    }, Out, []).

output:

    <h1>Hello</h1>

    <h2>Item 1</h2>
    <div class="content">Abc 1</div>

    <h2>Item 1</h2>
    <div class="content">Abc 2</div>

## Alternate syntax: semblance

If you find your tools work better with a template tag syntax that
more closely resembles other web templating engines, such as Django
(Python) / Djula (Common Lisp) / Twig (PHP), you can also use an
alternate frontend syntax called semblance:

Input markup (`test.html` file):

    <h1>{{ title }}</h1>
    {% each items, item %}
        <h2>{{ item.title }}</h2>
        <div class="content">{% unescape item.content %}</div>
    {% end %}

rendering with data:

    use_module(library(st/st_render)).

    current_output(Out),
    st_render_file(test, _{
        title: 'Hello',
        items: [
            _{ title: 'Item 1', content: 'Abc 1' },
            _{ title: 'Item 1', content: 'Abc 2' }
        ]
    }, Out, _{ frontend: semblance }).

output:

    <h1>Hello</h1>

    <h2>Item 1</h2>
    <div class="content">Abc 1</div>

    <h2>Item 1</h2>
    <div class="content">Abc 2</div>

It is not possible to mix two syntaxes through a single render call.

## API

Render template from string:

    st_render_string(String, Data, Stream, File, Options).

Render template from file:

    st_render_file(File, Data, Stream, Options).

Render template from codes:

    st_render_codes(Codes, Data, Stream, File, Options).

Options accept the following:

 * encoding - default `utf8`.
 * extension - file name extension, default `html`.
 * cache - whether to use cache, default `false`.
 * strip - whether to try to strip whitespace from output, default `false`.
 * frontend - which syntax to use, currently `simple` (default) or `semblance`.
 * undefined - throw error on undefined
   variables - `error` (default) or `false` (evaluates to false).

## Processing instructions

Processing instructions start with double opening curly braces (`{{`) and
end with double closing curly braces (`}}`). Semblance syntax variations
are described below.

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

### Blocks

Blocks allow to compose templates while passing around contained template as
a value. This allows to wrap templated content with another template.

Example 1: wrapping content into a panel element.

Input file:

    {{ block panel }}
        <span>This will be wrapped in panel.</span>
    {{ end }}

Panel file:

    <div class="panel">{{ slot }}</div>

Rendering result:

    <div class="panel"><span>This will be wrapped in panel.</span></div>

Variable scoping inside block content is lexical. Variable scope inside
block is either the current scope or selectable with an expression similar
to the `{{ include file, expr }}` instruction.

Example 2: layouts.

Layout file (`layout.html`):

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>{{= title }}</title>
        </head>
        <body>
            <h1>{{= title }}</h1>
            {{ slot }}
        </body>
    </html>

Concrete page file:

    {{ block layout }}
        <span>This is the page file.</span>
    {{ end }}

Data:

    _{ title: "A page title" }

Rendering result:

    <!DOCTYPE html>
    <html lang="en">
        <head>
            <meta charset="UTF-8">
            <title>{{= title }}</title>
        </head>
        <body>
            <h1>{{= title }}</h1>
            <span>This is the page file.</span>
        </body>
    </html>

## Semblance syntax

The semblance syntax has the following differences.

Output instruction: `{{ variable }}`.

Unescaped output: `{% unescape rawvar %}`.

Conditional instructions:

    {% if cond_expression1 %}
    ...
    {% else if cond_expression2 %}
    ...
    {% else %}
    ...
    {% end %}

Each loop:

    {% each expression, item_var, index_var, len_var %}
    ...
    {% end %}

Comments: `{# comment #}`.

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

## Undefined variables

The default behaviour when a variable that is not in the scope (template data dict) is encountered is to throw a `no_entry` error. This behaviour can be modified to set the variable value to `false` (default is `error`) by setting the `undefined` option to `false` when rendering templates. For example:

    st_render_file(test, _{
        title: 'Hello',
        items: [
            _{ title: 'Item 1', content: 'Abc 1' },
            _{ title: 'Item 1', content: 'Abc 2' }
        ]
    }, Out, _{ undefined: false }).

This is useful in situations where a developer may want to include a block in the template based on the existence of a variable. For example:

    {% if page_header %}
        <h2>{{page_header}}</h2>
    {% end %}

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

Template caching is enabled by setting option `cache: true` during rendering. This makes
the system cache parsed templates. This is particulary useful when using includes in loops.
To purge the current cache contents, use the `st_cache_invalidate` predicate.

## Whitespace removal

Some (but not all) whitespace is removed by setting option
`strip: true` during rendering. Line indents and some duplicate line ends
are removed from the output. Whitespace removal is parse-time and does not
incur any runtime penalty.

## Encoding

Encoding for template files can be specified by setting option `encoding` during rendering. Accepted
values are all that are accepted by the `encoding` option of the `read_file_to_codes/3` predicate.

## Installation

This package requires SWI-Prolog 7.x.

    pack_install(simple_template).

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

 * 2017-11-03 version 1.2.0. Option to deal with undefined variables.
 * 2015-12-28 version 1.1.0. Add alternate syntax: semblance.
 * 2015-11-07 version 1.0.0. Removal of global options. Backwards-incompatible.
 * 2014-05-09 version 0.3.0. Provide st_cache_invalidate/0, \= operator, comments.
 * 2014-05-07 version 0.2.0. Literal lists, encode_* functions.
 * 2014-03-02 version 0.1.0. Provided st_set_encoding/1.
 * 2014-01-30 version 0.0.1

## License

The MIT License. See LICENSE file.
