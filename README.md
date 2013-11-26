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

## TODO

 * Includes
 * Caching
 * API documentation

## License

The MIT License. See LICENSE file.
