-module(text_img_table_list).

-compile(export_all).

-import(erlguten, [get_tag_schema/2, parse_fontSize/1]).
-import(lists, [map/2]).
-include("mail_merge.hrl").

%% Something about table: currently the format info are either hard coded or ignored. But they can all be easily added into the template and later parsed with the functions in this module, so we don't need to worry that we might have to modify the erlguten module.


%%text

parse_text(Frame_info, Content, PDF) ->
  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Font = Frame_info#frame_info.y,
  
  {Pt, _} = 
  case Frame_info#frame_info.fontsize of
    "N/A" -> parse_fontSize("12/24");  %% temprarily
    FontSize_Value ->
      parse_fontSize(FontSize_Value)    
  end,

  Text = Content,

  pdf:save_state(PDF),
  pdf:begin_text(PDF),
  pdf:set_font(PDF,Font, Pt),
  pdf:set_text_rendering(PDF, fill),
  pdf:set_text_pos(PDF, X, Y),  
  pdf:textbr(PDF, Text),
  pdf:end_text(PDF),
  pdf:restore_state(PDF).


%%Images
parse_img(Frame_info, Content, PDF) ->              %% Temparily 
  parse_text(Frame_info, Content, PDF).

%%Table
parse_table(Frame_info, Content, PDF) ->
  Page_No = pdf:get_page_no(PDF),                            %%save the current page number
  parse_and_print_table(Frame_info, Content, PDF),
  pdf:set_page(PDF, Page_No).                                %%restore the former page number

parse_and_print_table(Frame_info, Content, PDF) ->
  [Format, TableContent] = string:tokens(Content, "@"),
  [Columns, Headers] = parseFormat(Format),
  RowsInList = parseTableContent(TableContent),
  printTable(Frame_info, PDF, {Columns, Headers, RowsInList}).

printHeaders(Frame_info, PDF, Headers) ->
  Font = Frame_info#frame_info.font,
  FontSize = Frame_info#frame_info.fontsize,
  {Pt,_} = parse_fontSize(FontSize),
  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Col_Width = 60, %% hard coded field, the unit is points (pixel?)
  printHeadersInLine(X, Y, Col_Width, Font, Pt, Headers, PDF).

printHeadersInLine(_, _, _, _, _, [], _) ->
  ok;
printHeadersInLine(X, Y, Col_Width, Font, Pt, [H|T], PDF) ->
  pdf:save_state(PDF),   
  pdf:begin_text(PDF),
  pdf:set_font(PDF,Font, Pt),
  pdf:set_text_rendering(PDF, fill),
  pdf:set_text_pos(PDF, X, Y),
  pdf:textbr(PDF, H),
  pdf:end_text(PDF),
  pdf:restore_state(PDF),
 
  printHeadersInLine(X + Col_Width, Y, Col_Width, Font, Pt, T, PDF).  %% It seems the length of word is not well computated

printRowInLine(_, _, _, _, _, [], _, _) ->
  ok;
printRowInLine(X, Y, Col_Width, Font, Pt, [H|T], OneRow, PDF) ->
  case lists:keysearch(H, 1, OneRow) of
    {value, {_, Value}} ->
      pdf:save_state(PDF),   
      pdf:begin_text(PDF),
      pdf:set_font(PDF,Font, Pt),
      pdf:set_text_rendering(PDF, fill),
      pdf:set_text_pos(PDF, X, Y),
      pdf:textbr(PDF, Value),
      pdf:end_text(PDF),
      pdf:restore_state(PDF),
      printRowInLine(X + Col_Width, Y, Col_Width, Font, Pt, T, OneRow, PDF);
    false ->
      io:format("Unknow internal error in Table printing procedure, columns mismatch~n"),
      erlang:halt()
  end.

printTable_aux(_, _, {_, []}) ->
  ok;
printTable_aux(Frame_info, PDF, {Columns, [H|T]}) ->
  Font = Frame_info#frame_info.font,
  FontSize = Frame_info#frame_info.fontsize,
  {Pt,Leading} = parse_fontSize(FontSize),
  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Col_Width = 60, %% hard coded field, the unit is points (pixel?)
  printRowInLine(X, Y, Col_Width, Font, Pt, Columns, H, PDF),

  Y1 = Frame_info#frame_info.y - Leading,  
  if 
    Y1 > Leading -> printTable_aux(Frame_info#frame_info{y = Y1}, PDF, {Columns, T});
    Y1 =< Leading andalso Y1 >= 0 ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
      pdf:new_page(PDF),
      printTable_aux(Frame_info#frame_info{y = Y1 - Leading + 842}, PDF, {Columns, T});
    Y1 < 0 ->                                          %% If Y1<0, it definitely belong to the next page
      pdf:new_page(PDF),
      printTable_aux(Frame_info#frame_info{y = Y1 + 842}, PDF, {Columns, T})
  end.

printTable(Frame_info, PDF, {Columns, [], RowsInList}) ->
  printTable_aux(Frame_info, PDF, {Columns, RowsInList});
printTable(Frame_info, PDF, {Columns, Headers, RowsInList}) ->
  printHeaders(Frame_info, PDF, Headers),
  FontSize = Frame_info#frame_info.fontsize,
  {_,Leading} = parse_fontSize(FontSize),

  Y1 = Frame_info#frame_info.y - Leading,  
  if 
    Y1 > Leading -> printTable_aux(Frame_info, PDF, {Columns, RowsInList});
    Y1 =< Leading andalso Y1 >= 0 ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
      pdf:new_page(PDF),
      %% Note that you need to move down TWO lines, why? it is a simple maths, just because move down one line will cause the text printed between pages
      printTable_aux(Frame_info#frame_info{y = Y1 - 2*Leading + 842}, PDF, {Columns, RowsInList});       
    Y1 < 0 ->                                          %% If Y1<0, it definitely belong to the next page
      %%io:format("y1<0~n"), %%debug
      pdf:new_page(PDF),
      printTable_aux(Frame_info#frame_info{y = Y1 + 842}, PDF, {Columns, RowsInList})
  end.

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

%%List
parse_list(Frame_info, Content, PDF) ->
  Page_No = pdf:get_page_no(PDF),                            %%save the current page number
  parse_and_print_list(Frame_info, Content, PDF),
  pdf:set_page(PDF, Page_No).                                %%restore the former page number

parse_and_print_list(Frame_info, Content, PDF) ->
  case re:split(Content, "{ul}|{li}|{/li}|{/ul}", [{return, list}]) of 
    Items -> 
      printList(Frame_info, Items, PDF)
  end.

printList(_, [], _) ->
  ok;
printList(Frame_info, [H|T], PDF) ->
  case H of
    [] ->printList(Frame_info, T, PDF);
    Str ->
      Font = Frame_info#frame_info.font,
      FontSize = Frame_info#frame_info.fontsize,
      {Pt,Leading} = parse_fontSize(FontSize),
      X = Frame_info#frame_info.x,
      Y = Frame_info#frame_info.y,
      
      pdf:save_state(PDF),   
      pdf:begin_text(PDF),
      pdf:set_font(PDF,Font, Pt),
      pdf:set_text_rendering(PDF, fill),
      pdf:set_text_pos(PDF, X, Y),
      %%pdf:image(PDF, "img/bullet_dot.gif"),           %%deal with this later
      pdf:textbr(PDF, Str),
      pdf:end_text(PDF),
      pdf:restore_state(PDF), 
      
      Y1 = Frame_info#frame_info.y - Leading,  
      if 
        Y1 > Leading -> printList(Frame_info#frame_info{y = Y1}, T, PDF);
        Y1 =< Leading andalso Y1 >= 0 ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
          pdf:new_page(PDF),
          printList(Frame_info#frame_info{y = Y1 - Leading + 842}, T, PDF);
        Y1 < 0 ->                                          %% If Y1<0, it definitely belong to the next page
          pdf:new_page(PDF),
          printList(Frame_info#frame_info{y = Y1 + 842}, T, PDF)
      end
  end.
