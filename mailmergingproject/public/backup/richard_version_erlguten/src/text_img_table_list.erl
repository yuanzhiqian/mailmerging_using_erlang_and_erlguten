-module(text_img_table_list).

-compile(export_all).

-import(from_old_erlguten, [parse_color/1, parse_fontSize/1, zip1/2]).
-import(lists, [map/2, flatten/1]).

-include("mail_merge.hrl").

%% Something about table: currently the format info are either hard coded or ignored. But they can all be easily added into the template and later parsed with the functions in this module, so we don't need to worry that we might have to modify the erlguten module.


%%text

parse_text(Frame_info, Content, _Merged, PDF) ->      %% Merged |   #template_info{  counts, {paper, Attr, [{frame_info, Content}| ... ]}  }
  Page_No = eg_pdf:get_page_no(PDF),                            %%save the current page number

  X = Frame_info#frame_info.x,
  Y = Frame_info#frame_info.y,
  Height = Frame_info#frame_info.height,
  Width = Frame_info#frame_info.width,

  case parse_color(Frame_info#frame_info.bg) of
	no ->
	    void;
	{yes,{R,G,B}} ->            
	    eg_pdf:save_state(PDF),
	    eg_pdf:set_fill_color_RGB(PDF,R, G, B),
	    eg_pdf:rectangle(PDF, X-5,Y-Height-12,10+Width,15 + Height, fill),
	    eg_pdf:restore_state(PDF)
  end,
  {_, Leading} = drawText(Frame_info, Content, PDF),
  case Frame_info#frame_info.grid of
	true ->
            %%io:format("~p~n~p~n~p~n~p~n~p~n",[X,Y,round(Width/12),Leading,Frame_info#frame_info.maxlines]),
	    P = erlguten_geometry:draw_box(X,Y,round(Width/12),Leading,Frame_info#frame_info.maxlines),
	    eg_pdf:append_stream(PDF, P);
	false ->
	    void
  end,
  
  eg_pdf:set_page(PDF, Page_No).                                %%restore the former page number

%%%%%%%% In this function, I reuse the codes from erlguten to print texts
drawText(Frame_info, Content, PDF) ->
  TagMap = [{raw, Frame_info#frame_info.font, Frame_info#frame_info.break}],
  {Pt, Leading} = 
  case Frame_info#frame_info.fontsize of
    "N/A" -> parse_fontSize("12/24");  %% temprarily
    FontSize_Value ->
      parse_fontSize(FontSize_Value)    
  end,
  FontMap = map(fun({Tg,Name,Bool}) ->
			  {Tg,get_font_alias(PDF, Name),Bool,Name}
		  end, TagMap),
  P1 = Frame_info#frame_info.paraIndent,
  Measure = round (Frame_info#frame_info.width / 12),
  ParaShape = map(fun(I) -> Measure-I end, P1),
  PointSize = Pt,
  Xml = {p,[], [{raw, Content}]},               %% In order to be able to use the functions, I need to construct the data structure needed
  Toks  = erlguten_normalise_xml:normalise_xml(Xml, FontMap),
  Lines = erlguten_para_break:break_para(Toks, PointSize, ParaShape, FontMap),
  PdfLines = erlguten_lines2pdf:lines2pdf(Lines, PointSize, ParaShape, FontMap),

  Need = length(Lines),
  Free = 1,
  Max  = Frame_info#frame_info.maxlines,
  Available = Max - Free + 1,
 
  case Need =< Available of 
    true ->
	    %% io:format("Good no worries~n"),
	    X = Frame_info#frame_info.x,
            Y = Frame_info#frame_info.y,
	    Y1 = Y-Leading - (Free-1)*Leading,
	    Geom = erlguten_geometry:mk_line_headers(X, Y1, Measure, 
						Leading, ParaShape, Need),
	    Pdf1 = ["BT\n", zip1(Geom, PdfLines), "ET\n"],
            
	    Pdf2 = flatten(Pdf1),
	    eg_pdf:append_stream(PDF, Pdf2);
    false ->
	    %% io:format("Oh dear~n")
	    void
  end,
  {Pt, Leading}.


%%Images
parse_img(Frame_info, Content, Merged, PDF) ->              %% Temparily 
  %%parse_text(Frame_info, Content, Merged, PDF).
  ok.

%%Table
parse_table(Frame_info, Content, Merged, PDF) ->
  Page_No = eg_pdf:get_page_no(PDF),                            %%save the current page number
  parse_and_print_table(Frame_info, Content, Merged, PDF),
  eg_pdf:set_page(PDF, Page_No).                                %%restore the former page number

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
  eg_pdf:save_state(PDF),   
  eg_pdf:begin_text(PDF),
  eg_pdf:set_font(PDF,Font, Pt),
  eg_pdf:set_text_rendering(PDF, fill),
  eg_pdf:set_text_pos(PDF, X, Y),
  eg_pdf:textbr(PDF, H),
  eg_pdf:end_text(PDF),
  eg_pdf:restore_state(PDF),
 
  printHeadersInLine(X + Col_Width, Y, Col_Width, Font, Pt, T, PDF).  %% It seems the length of word is not well computated

printRowInLine(_, _, _, _, _, [], _, _) ->
  ok;
printRowInLine(X, Y, Col_Width, Font, Pt, [H|T], OneRow, PDF) ->
  case lists:keysearch(H, 1, OneRow) of
    {value, {_, Value}} ->
      eg_pdf:save_state(PDF),   
      eg_pdf:begin_text(PDF),
      eg_pdf:set_font(PDF,Font, Pt),
      eg_pdf:set_text_rendering(PDF, fill),
      eg_pdf:set_text_pos(PDF, X, Y),
      eg_pdf:textbr(PDF, Value),
      eg_pdf:end_text(PDF),
      eg_pdf:restore_state(PDF),
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
      eg_pdf:new_page(PDF),

      Page_No = eg_pdf:get_page_no(PDF),

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
      eg_pdf:new_page(PDF),
      Page_No = eg_pdf:get_page_no(PDF),

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
  Page_No = eg_pdf:get_page_no(PDF),                            %%save the current page number
  parse_and_print_list(Frame_info, Content, PDF),
  eg_pdf:set_page(PDF, Page_No).                                %%restore the former page number

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
      %%Font = Frame_info#frame_info.font,
      FontSize = Frame_info#frame_info.fontsize,
      {_Pt,Leading} = parse_fontSize(FontSize),
      %%X = Frame_info#frame_info.x,
      %%Y = Frame_info#frame_info.y,     

      %%eg_pdf:save_state(PDF),   
      %%eg_pdf:begin_text(PDF),
      %%eg_pdf:set_font(PDF,Font, Pt),
      %%eg_pdf:set_text_rendering(PDF, fill),
      %%%eg_pdf:set_text_pos(PDF, X, Y),
      %%eg_pdf:image(PDF, "img/bullet_dot.gif"),           %%deal with this later
      %%eg_pdf:textbr(PDF, Str),
      %%eg_pdf:end_text(PDF),
      %%eg_pdf:restore_state(PDF), 

      drawText(Frame_info, Str, PDF),  %% using erlguten functions to draw text, instead of directly using pdf module
      
      Y1 = Frame_info#frame_info.y - Leading,  
      if 
        Y1 > Leading -> printList(Frame_info#frame_info{y = Y1}, T, PDF);
        Y1 =< Leading andalso Y1 >= 0 ->                   %% If the last line in the page has a larger height than the space left, then move it to the next page
          eg_pdf:new_page(PDF),
          printList(Frame_info#frame_info{y = Y1 - Leading + 842}, T, PDF);
        Y1 < 0 ->                                          %% If Y1<0, it definitely belong to the next page
          eg_pdf:new_page(PDF),
          printList(Frame_info#frame_info{y = Y1 + 842}, T, PDF)
      end
  end.
