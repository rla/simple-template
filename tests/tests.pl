:- begin_tests(st_render).

:- use_module(library(debug)).
:- use_module(prolog/st/st_render).

:- meta_predicate(test_rendering(0, +)).

test_rendering(Goal, Expected):-
    with_output_to_codes(Goal, Codes),
    atom_codes(Atom, Codes),
    assertion(Atom = Expected).

test(empty):-
    test_rendering(st_render_codes("", [], ''), '').
    
test(text):-
    test_rendering(st_render_codes("text", [], ''), 'text').
    
test(variable):-
    test_rendering(st_render_codes("[[= a ]]", [a(123)], ''), '123').
    
test(escape):-
    test_rendering(st_render_codes("[[= a ]]", [a('<')], ''), '&lt;').
    
test(nonescape):-
    test_rendering(st_render_codes("[[- a ]]", [a('<')], ''), '<').
    
test(condition1_true):-
    test_rendering(st_render_codes("[[? if(a=1) ]]t[[?]]", [a(1)], ''), 't').
    
test(condition1_false):-
    test_rendering(st_render_codes("[[? if(a=1) ]]t[[?]]", [a(2)], ''), '').
    
test(condition2_true):-
    test_rendering(st_render_codes("[[? if(a=1) ]]t[[? else ]]f[[?]]", [a(1)], ''), 't').
    
test(condition2_false):-
    test_rendering(st_render_codes("[[? if(a=1) ]]t[[? else ]]f[[?]]", [a(2)], ''), 'f').
    
test(condition3_true):-
    test_rendering(st_render_codes("[[? if(a=\"1\") ]]t[[? else ]]f[[?]]", [a('1')], ''), 't').
    
test(condition3_false):-
    test_rendering(st_render_codes("[[? if(a=\"1\") ]]t[[? else ]]f[[?]]", [a('2')], ''), 'f').
    
test(each):-
    test_rendering(st_render_codes("[[: each(items, item) ]][[= item ]][[:]]", [items([1,2,3])], ''), '123').
    
test(each_index):-
    test_rendering(st_render_codes("[[: each(items, item, i) ]][[= item ]][[= i ]][[:]]", [items([1,2,3])], ''), '102132').
    
test(include):-
    test_rendering(st_render_codes("[[* include(tests/included) ]]", [], 'dummy.html'), 'i').
    
test(include_variable):-
    test_rendering(st_render_codes("[[* include(tests/included_variable) ]]", [a(1)], 'dummy.html'), '1').
    
test(include_variable_scoped):-
    test_rendering(st_render_codes("[[* include(tests/included_variable, b) ]]", [b([a(1)])], 'dummy.html'), '1').

:- end_tests(st_render).
