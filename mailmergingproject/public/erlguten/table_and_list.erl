-module(table_and_list).

-compile(export_all).

-import(erlguten, [get_tag_schema/2, parse_fontSize/1]).
-import(lists, [map/2]).
-include("erlguten.hrl").

parse_table({Tag,[],[{SubTag,[],[{raw,Data}]}]}, Box1, PDF) ->
  {Tag,[],[{SubTag,[],[{raw,Data}]}]}.              %% Temporarily use a placeholder

parse_list({Tag,[],[{SubTag,[],[{raw,Data}]}]}, Box1, PDF) ->
  parse_and_print_list(Tag, SubTag, Data, Box1, PDF),
  {Tag,[],[{SubTag,[],[{raw,""}]}]}.               %% Temporarily use a placeholder

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
