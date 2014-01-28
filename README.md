# simple-template

Text (including HTML) template processor for Prolog.

## Example

Input file:

    <!DOCTYPE html>
    <html>
        <head>
            <title>[[=title]]</title>
        </head>
        <body>
            [[:each(items, item)]]            
                <h1>[[=item:title]]</h1>            
                <div class="content">[[-item:content]]</div>
            [[:]]
        </body>
    </html>

Rendering:

    st_render_file('test.html', [
        title('Hello'),
        items([
            [title('Item 1'), content('Abc 1')],
            [title('Item 2'), content('Abc 2')]
        ])
    ]).

Output:

    <!DOCTYPE html>
    <html>
        <head>
            <title>Hello</title>
        </head>
        <body>
                        
                <h1>Item 1</h1>            
                <div class="content">Abc 1</div>
                        
                <h1>Item 2</h1>            
                <div class="content">Abc 2</div>
            
        </body>
    </html>

### Expressions

Expressions are ground Prolog terms with the interpretation
described below.

In most contexts, atoms are interpreted as scope entries ("variables").
An error is thrown when there is no suitable entry.

The following arithmetic operators and functions are supported:

Boolean expressions include modified unification using the `=` operator. The
operator provides coercion from atom to string when either of operands is an atom
and the other is a string. For other types, no coercion is applied. Supported boolean
operators are: ``.

Conditional expression `if(expression, true_expr, false_expr)` value is the value of
`true_expr` when the `expression` evaluates to anything but 0. Otherwise the
value is the value of `false_expr`.

The `+` operator is overloaded to provide string or atom concatenation when
the left side of the operator is an atom or string.

Compound terms that match none of the operators and constructs described above are
treated as user-defined function calls. An error is thrown when there is no such function
defined (see below).

### Includes

Includes use the syntax `{{ include(path/to/file) }}`. The path should be relative.
The relative path is resolved against the current file location and the file
extension is added. Includes are
processed at runtime. This means that recursive includes are possible provided
that they are wrapped into if/else block that terminates the recursion at some point.

### Global constants

Global constants can be set with the `st_set_global/2`. When a scope contains an
entry with the same name as a global then the scope value is preferred.

### User-defined functions

### HTML escaping

HTML escaping is automatically applied to output statements beginning with the `{{=` token.
Escaped output is safe to be used inside HTML elements and attribute values. There must be
no space between `{{` and `=`.

### Caching

## TODO

 * Includes
 * Caching
 * API documentation

## License

The MIT License. See LICENSE file.
