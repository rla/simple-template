:- begin_tests(st_tokens_user_defined).

:- use_module(prolog/st/st_tokens).

% Syntax similar to embedded ruby / ERB
erblike_frontend(
    syntax_tokens(
        comment("<%#", "%>"),
        out("<%=", "%>"),
        out_unescaped(keyword_unescape_start("<%="), "%>"),
        statement("<%", "%>"))).

test(text):-
    erblike_frontend(Frontend),
    st_tokens(`abc`,
        _{ frontend: Frontend }, [text(`abc`)]).

test(out):-
    erblike_frontend(Frontend),
    st_tokens(`<%= abc %>`,
        _{ frontend: Frontend }, [out(abc)]).

test(out_unescaped):-
  erblike_frontend(Frontend),
  st_tokens(`<%= unescape abc %>`,
      _{ frontend: Frontend }, [out_unescaped(abc)]).

test(out_unescaped_without_keyword):-
  st_tokens(`<%~ abc %>`,
      _{ frontend: syntax_tokens(
          comment("<%#", "%>"),
          out("<%=", "%>"),
          out_unescaped("<%~", "%>"),
          statement("<%", "%>"))
      }, [out_unescaped(abc)]).

test(end):-
    erblike_frontend(Frontend),
    st_tokens(`<% end %>`,
        _{ frontend: Frontend }, [end]).

test(else):-
    erblike_frontend(Frontend),
    st_tokens(`<% else %>`,
        _{ frontend: Frontend }, [else]).

test(include):-
    erblike_frontend(Frontend),
    st_tokens(`<% include file/name %>`,
        _{ frontend: Frontend }, [include(file/name)]).

test(include_var):-
    erblike_frontend(Frontend),
    st_tokens(`<% include file/name, var %>`,
        _{ frontend: Frontend }, [include(file/name, var)]).

test(dynamic_include):-
    erblike_frontend(Frontend),
    st_tokens(`<% dynamic_include var %>`,
        _{ frontend: Frontend }, [dynamic_include(var)]).

test(block):-
    erblike_frontend(Frontend),
    st_tokens(`<% block file/name %>`,
        _{ frontend: Frontend }, [block(file/name)]).

test(block_var):-
    erblike_frontend(Frontend),
    st_tokens(`<% block file/name, var %>`,
        _{ frontend: Frontend }, [block(file/name, var)]).

test(slot):-
    erblike_frontend(Frontend),
    st_tokens(`<% slot %>`,
        _{ frontend: Frontend }, [slot]).

test(if):-
    erblike_frontend(Frontend),
    st_tokens(`<% if x=1 %>`,
        _{ frontend: Frontend }, [if(x=1)]).

test(else_if):-
    erblike_frontend(Frontend),
    st_tokens(`<% else if x=2 %>`,
        _{ frontend: Frontend }, [else_if(x=2)]).

test(each_1):-
    erblike_frontend(Frontend),
    st_tokens(`<% each items, item %>`,
        _{ frontend: Frontend }, [each(items, item)]).

test(each_2):-
    erblike_frontend(Frontend),
    st_tokens(`<% each items, item, index %>`,
        _{ frontend: Frontend }, [each(items, item, index)]).

test(each_3):-
    erblike_frontend(Frontend),
    st_tokens(`<% each items, item, index, len %>`,
        _{ frontend: Frontend }, [each(items, item, index, len)]).

test(invalid):-
    erblike_frontend(Frontend),
    catch((st_tokens(`<% invalid`,
        _{ frontend: Frontend }, _), fail),
        error(invalid_instruction(_)), true).

test(nonground):-
    erblike_frontend(Frontend),
    catch((st_tokens(`<%= A %>`,
        _{ frontend: Frontend }, _), fail),
        error(non_ground_expression(_)), true).

test(comment):-
    erblike_frontend(Frontend),
    st_tokens(`<%# this is a comment #%>`,
        _{ frontend: Frontend }, []).

:- end_tests(st_tokens_user_defined).
