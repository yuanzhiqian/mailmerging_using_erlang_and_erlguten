-module(table_and_list).

-compile(export_all).

-import(erlguten, [get_tag_schema/2, parse_fontSize/1]).
-import(lists, [map/2]).
-include("erlguten.hrl").

%% Something about table: currently the format info are either hard coded or ignored. But they can all be easily added into the template and later parsed with the functions in this module, so we don't need to worry that we might have to modify the erlguten module.

parse_table({Tag,[],[{SubTag,[],[{raw,Data}]}]}, Box1, PDF) ->
  parse_and_print_table(Tag, SubTag, Data, Box1, PDF),
  {Tag,[],[{SubTag,[],[{raw,""}]}]}.              %% Temporarily use a placeholder -- Placeholder Removed

parse_and_print_table(Tag, SubTag, Data, Box1, PDF) ->
  [Format, TableContent] = string:tokens(Data, "@"),
  %%io:format("~p~n~p~n", [Format, TableContent])  %%debug
  [Columns, Headers] = parseFormat(Format),
  %io:format("~p~n~p~n", [Columns, Headers]),  %%debug
  RowsInList = parseTableContent(TableContent),
  %%io:format("~p~n", [RowsInList]),  %%debug
  printTable(Tag, SubTag, Box1, PDF, {Columns, Headers, RowsInList}).

printHeaders(Tag, SubTag, Box1, PDF, Headers) ->
  CurrentObj = get_tag_schema(atom_to_list(Tag), Box1#box.objs),
  TagMap = CurrentObj#obj.tags,
  FontMap = map(fun({Tg,Name,Bool}) ->
		  {Tg,pdf:get_font_alias(PDF, Name),Bool,Name}
	        end, TagMap),
  %%io:format("~p~n", [FontMap]), %%debug
  Font = case lists:keysearch(SubTag, 1, FontMap) of
    {value, {raw,_,_,Font_found}} ->
      Font_found;
    false ->
      io:format("Unknow internal error in printList function!~n~p~n", [SubTag]),
      erlang:halt()
  end,
  P1 = CurrentObj#obj.paraIndent,
  Measure = Box1#box.measure, 
  ParaShape = map(fun(I) -> Measure-I end, P1),
  FontSize = Box1#box.fontSize,
  {Pt,Leading} = parse_fontSize(FontSize),
  X = Box1#box.x,
  Y = Box1#box.y,
  Col_Width = 60, %% hard coded field, the unit is points (pixel?)
  printHeadersInLine(X, Y, Col_Width, Font, FontSize, Pt, Headers, PDF).

printHeadersInLine(_, _, _, _, _, _, [], _) ->
  ok;
printHeadersInLine(X, Y, Col_Width, Font, FontSize, Pt, [H|T], PDF) ->
  pdf:save_state(PDF),   
  pdf:begin_text(PDF),
  pdf:set_font(PDF,Font, Pt),
  pdf:set_text_rendering(PDF, fill),
  pdf:set_text_pos(PDF, X, Y),
  %%pdf:image(PDF, "img/bullet_dot.gif"),           %%deal with this later
  pdf:textbr(PDF, H),
  pdf:end_text(PDF),
  pdf:restore_state(PDF),
 
  printHeadersInLine(X + Col_Width, Y, Col_Width, Font, FontSize, Pt, T, PDF).  %% It seems the length of word is not well computated

printRowInLine(_, _, _, _, _, _, [], _, _) ->
  ok;
printRowInLine(X, Y, Col_Width, Font, FontSize, Pt, [H|T], OneRow, PDF) ->
  case lists:keysearch(H, 1, OneRow) of
    {value, {_, Value}} ->
      pdf:save_state(PDF),   
      pdf:begin_text(PDF),
      pdf:set_font(PDF,Font, Pt),
      pdf:set_text_rendering(PDF, fill),
      pdf:set_text_pos(PDF, X, Y),
      %%pdf:image(PDF, "img/bullet_dot.gif"),           %%deal with this later
      pdf:textbr(PDF, Value),
      pdf:end_text(PDF),
      pdf:restore_state(PDF),
      printRowInLine(X + Col_Width, Y, Col_Width, Font, FontSize, Pt, T, OneRow, PDF);
    false ->
      io:format("Unknow internal error in Table printing procedure, columns mismatch~n"),
      erlang:halt()
  end.

printTable_aux(Tag, SubTag, Box1, PDF, {Columns, []}) ->
  ok;
printTable_aux(Tag, SubTag, Box1, PDF, {Columns, [H|T]}) ->
  CurrentObj = get_tag_schema(atom_to_list(Tag), Box1#box.objs),
  TagMap = CurrentObj#obj.tags,
  FontMap = map(fun({Tg,Name,Bool}) ->
		  {Tg,pdf:get_font_alias(PDF, Name),Bool,Name}
	        end, TagMap),
  %%io:format("~p~n", [FontMap]), %%debug
  Font = case lists:keysearch(SubTag, 1, FontMap) of
    {value, {raw,_,_,Font_found}} ->
      Font_found;
    false ->
      io:format("Unknow internal error in printList function!~n~p~n", [SubTag]),
      erlang:halt()
  end,
  P1 = CurrentObj#obj.paraIndent,
  Measure = Box1#box.measure, 
  ParaShape = map(fun(I) -> Measure-I end, P1),
  FontSize = Box1#box.fontSize,
  {Pt,Leading} = parse_fontSize(FontSize),
  X = Box1#box.x,
  Y = Box1#box.y,
  Col_Width = 60, %% hard coded field, the unit is points (pixel?)
  printRowInLine(X, Y, Col_Width, Font, FontSize, Pt, Columns, H, PDF),
  printTable_aux(Tag, SubTag, Box1#box{y = Box1#box.y - Leading}, PDF, {Columns, T}).
printTable(Tag, SubTag, Box1, PDF, {Columns, [], RowsInList}) ->
  printTable_aux(Tag, SubTag, Box1, PDF, {Columns, RowsInList});
printTable(Tag, SubTag, Box1, PDF, {Columns, Headers, RowsInList}) ->
  printHeaders(Tag, SubTag, Box1, PDF, Headers),
  FontSize = Box1#box.fontSize,
  {_,Leading} = parse_fontSize(FontSize),
  printTable_aux(Tag, SubTag, Box1#box{y = Box1#box.y - Leading}, PDF, {Columns, RowsInList}).

parseFormat(Format) ->
  Columns = case re:run(Format, "columns[ ]+=[ ]+{[a-zA-Z0-9_, ]+}", []) of
    {match, [{Start, Len}]} ->
      ColStr = lists:sublist(Format, Start+1, Len),
      case re:split(ColStr, "columns[ ]+=[ ]+{|}| |,", [{return, list}]) of
        Columns_raw ->
          deEmptyLists(Columns_raw)
      end;
    nomatch -> 
      io:format("Template has a bad format in table definition, or the parseFormat function is flawed.~n"),
      erlang:halt()
  end,
  Headers = case re:split(Format, "{table[ ]+columns[ ]+=[ ]+{[a-zA-Z0-9, ]+}[ ]*}|{tr}|{th}|{/th}|{/tr}|{/table}", [{return, list}]) of
    Headers_raw ->
      deEmptyLists(Headers_raw)
  end,
  [Columns, Headers].

deEmptyLists_aux([], Acc) ->
  lists:reverse(Acc);
deEmptyLists_aux([[]|T], Acc) ->
  deEmptyLists_aux(T, Acc);
deEmptyLists_aux([H|T], Acc) ->
  deEmptyLists_aux(T, [H|Acc]).

deEmptyLists(Columns_raw) ->
  deEmptyLists_aux(Columns_raw, []).

parseTableContent(TableContent) ->
  {ok, Tokens, _} = erl_scan:string(TableContent),
  %%io:format("~p~n", [TableContent]),  %%debug
  {ok, Term} = erl_parse:parse_term(Tokens),
  Term.

parse_list({Tag,[],[{SubTag,[],[{raw,Data}]}]}, Box1, PDF) ->
  parse_and_print_list(Tag, SubTag, Data, Box1, PDF),
  {Tag,[],[{SubTag,[],[{raw,""}]}]}.               %% Temporarily use a placeholder -- Placeholder Removed

parse_and_print_list(Tag, SubTag, Data, Box1, PDF) ->
  case re:split(Data, "{ul}|{li}|{/li}|{/ul}", [{return, list}]) of 
    Items -> 
      printList(Tag, SubTag, Items, Box1, PDF)
  end.

printList(_, _, [], _, _) ->
  ok;
printList(Tag, SubTag, [H|T], Box1, PDF) ->
  case H of
    [] ->printList(Tag, SubTag, T, Box1, PDF);
    Str ->
      CurrentObj = get_tag_schema(atom_to_list(Tag), Box1#box.objs),
      TagMap = CurrentObj#obj.tags,
      FontMap = map(fun({Tg,Name,Bool}) ->
			  {Tg,pdf:get_font_alias(PDF, Name),Bool,Name}
		  end, TagMap),
      %%io:format("~p~n", [FontMap]), %%debug
      Font = case lists:keysearch(SubTag, 1, FontMap) of
        {value, {raw,_,_,Font_found}} ->
          Font_found;
        false ->
          io:format("Unknow internal error in printList function!~n~p~n", [SubTag]),
          erlang:halt()
      end,
      P1 = CurrentObj#obj.paraIndent,
      Measure = Box1#box.measure, 
      ParaShape = map(fun(I) -> Measure-I end, P1),
      FontSize = Box1#box.fontSize,
      {Pt,Leading} = parse_fontSize(FontSize),
      X = Box1#box.x,
      Y = Box1#box.y,
      
      pdf:save_state(PDF),   
      pdf:begin_text(PDF),
      pdf:set_font(PDF,Font, Pt),
      pdf:set_text_rendering(PDF, fill),
      pdf:set_text_pos(PDF, X, Y),
      %%pdf:image(PDF, "img/bullet_dot.gif"),           %%deal with this later
      pdf:textbr(PDF, Str),
      pdf:end_text(PDF),
      pdf:restore_state(PDF),
 
      NewBox = Box1#box{y = Y - Leading},
      printList(Tag, SubTag, T, NewBox, PDF)
  end.
