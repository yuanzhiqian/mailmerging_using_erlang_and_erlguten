-module(text_img_table_list).

-compile(export_all).

-import(erlguten, [get_tag_schema/2, parse_fontSize/1]).
-import(lists, [map/2]).
-include("mail_merge.hrl").

%% Something about table: currently the format info are either hard coded or ignored. But they can all be easily added into the template and later parsed with the functions in this module, so we don't need to worry that we might have to modify the erlguten module.


%%text

parse_text(Frame_info, Content, _Merged, PDF) ->      %% Merged |   #template_info{  counts, {paper, Attr, [{frame_info, Content}| ... ]}  }
  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Font = case Frame_info#frame_info.font of
    "N/A" -> "Times-Roman";
    Real_Font -> Real_Font
  end,

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
parse_img(Frame_info, Content, Merged, PDF) ->              %% Temparily 
  parse_text(Frame_info, Content, Merged, PDF).

%%Table
parse_table(Frame_info, Content, Merged, PDF) ->
  Page_No = pdf:get_page_no(PDF),                            %%save the current page number
  parse_and_print_table(Frame_info, Content, Merged, PDF),
  pdf:set_page(PDF, Page_No).                                %%restore the former page number

parse_and_print_table(Frame_info, Content, Merged, PDF) ->
  case re:run(Content, "^@[a-zA-Z0-9_~!@#$%^&*()]+", []) of
    nomatch ->
      [Format, TableContent] = string:tokens(Content, "@"),
      [Columns, Headers] = parseFormat(Format),
      RowsInList = parseTableContent(TableContent),
      [MidPage_ContinueTable, EndPage_ContinueTable] = findContinueTable(Frame_info, Merged),
      printTable([Frame_info, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, Headers, RowsInList});
    {match, _} ->
      ok
  end. 
  

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

printTable_aux(_, _, _, {_, []}) ->
  ok;
printTable_aux([Frame_info, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, [H|T]}) ->
  Font = Frame_info#frame_info.font,
  FontSize = Frame_info#frame_info.fontsize,
  {Pt,Leading} = parse_fontSize(FontSize),
  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Col_Width = 60, %% hard coded field, the unit is points (pixel?)
  printRowInLine(X, Y, Col_Width, Font, Pt, Columns, H, PDF),

  Y1 = Frame_info#frame_info.y - Leading,  
  if 
    Y1 > Leading -> printTable_aux([Frame_info#frame_info{y = Y1}, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, T});
    Y1 =< Leading ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
      pdf:new_page(PDF),

      Page_No = pdf:get_page_no(PDF),

      Continue_table = 
      if Page_No == Merged#template_info.page_amount_needed -> 
           printEndPage(Merged#template_info.end_paper, Merged, PDF),
           EndPage_ContinueTable;
         Page_No =/= Merged#template_info.page_amount_needed -> 
           printMidPage(Merged#template_info.middle_paper, Merged, PDF),
           MidPage_ContinueTable
      end,
      %%io:format("~p~n", [Continue_table#frame_info.name]),
      printTable_aux([Continue_table, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, T})
  end.

printTable([Frame_info, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, [], RowsInList}) ->
  printTable_aux([Frame_info, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, RowsInList});
printTable([Frame_info, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, Headers, RowsInList}) ->
  printHeaders(Frame_info, PDF, Headers),
  FontSize = Frame_info#frame_info.fontsize,
  {_,Leading} = parse_fontSize(FontSize),

  Y1 = Frame_info#frame_info.y - Leading,  
  %%io:format("~p~n~p~n", [Frame_info#frame_info.y, Y1]),
  if 
    Y1 > Leading -> printTable_aux([Frame_info#frame_info{y = Y1}, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, RowsInList});
    Y1 =< Leading ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
      pdf:new_page(PDF),
      Page_No = pdf:get_page_no(PDF),

      Continue_table = 
      if Page_No == Merged#template_info.page_amount_needed -> 
           printEndPage(Merged#template_info.end_paper, Merged, PDF),
           EndPage_ContinueTable;
         Page_No =/= Merged#template_info.page_amount_needed -> 
           printMidPage(Merged#template_info.middle_paper, Merged, PDF),
           MidPage_ContinueTable
      end,
      %%io:format("~p~n", [Continue_table#frame_info.name]),
      printTable_aux([Continue_table, MidPage_ContinueTable, EndPage_ContinueTable], Merged, PDF, {Columns, RowsInList})
  end.

printEndPage({paper, _, Data}, Merged, PDF) ->
  printPage(Data, Merged, PDF).

printMidPage({paper, _, Data}, Merged, PDF) ->
  printPage(Data, Merged, PDF).

printPage(Data, Merged, PDF) ->
  mail_merge:generatePDF_for_each_page(Data, Merged, PDF).

findContinueTable(Frame_info, Merged) ->
  MidPage_ContinueTable = 
  case Frame_info#frame_info.continue of
    ["none"] -> 
      io:format("No continue table error~n"),
      halt();
    [Mid_name] ->
      findTable(Mid_name, Merged)
  end,
  EndPage_ContinueTable = 
  case MidPage_ContinueTable#frame_info.continue of
    ["none"] -> 
      io:format("No continue table error~n"),
      halt();
    [End_name] ->
      findTable(End_name, Merged)
  end,
  [MidPage_ContinueTable, EndPage_ContinueTable].

findTable(Name, Merged) ->
  %%io:format("~p~n", [Merged]),
  Frame_list = 
  case Merged#template_info.counts of
    1 ->
      {paper, _, PF} = Merged#template_info.front_paper,
      make_frame_list([PF]);
    2 ->
      {paper, _, PF} = Merged#template_info.front_paper,      
      {paper, _, PE} = Merged#template_info.end_paper,
      make_frame_list([PF, PE]);
    3 ->
      {paper, _, PF} = Merged#template_info.front_paper,
      {paper, _, PM} = Merged#template_info.middle_paper,
      {paper, _, PE} = Merged#template_info.end_paper,
      make_frame_list([PF, PM, PE])
  end,    

  L = lists:map(fun(F) -> if F#frame_info.name == Name -> F; F#frame_info.name =/= Name -> [] end end, Frame_list),  
  [TableFrame] = lists:flatten(L),
  TableFrame.

make_frame_list(L) ->
  lists:map(fun({F, _}) -> F end, lists:flatten(L)).

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
parse_list(Frame_info, Content, _Merged, PDF) ->
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
