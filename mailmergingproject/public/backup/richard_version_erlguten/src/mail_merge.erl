%%===============================================================================================================================================================
%% mail merge
%%---------------------------------------------------------------------------------------------------------------------------------------------------------------
%% Notice: 1. The output ommits redundant spaces, however, this is not my aim, it is the erlguten that does this on purpose. So I'm afraid I can not retain redundant spaces
%%         2. #name means the param "name", and the ##name means the string "#name", we don't allow parameters with the name beginning with #
%%---------------------------------------------------------------------------------------------------------------------------------------------------------------
%% Author: Yuan Zhiqian
%%
%%===============================================================================================================================================================

-module(mail_merge).
-compile(export_all).

-include("mail_merge.hrl").

-import(from_old_erlguten, [parse_fontSize/1, to_bool/1, parse_paraIndent/1]).
-import(eg_xml_lite, [parse_file/1]).

main(ArgList) ->
  [Template, UserData] =  ArgList, 
  T = parse_file(Template),
  U = parse_file(UserData),
  
  T_Data = deShell(T),  
  U_Data = deShell(U),
  
  %% The count indicates the number of papers defined, i.e. whether all front, middle and end are defined or only some of them are defined. And the alt2 ,alt3 
  %%     shows the template to use if current template can't hold all the contents
  {template, [{"alt2",Alt2}, {"alt3",Alt3},{"count",Count}], T_Papers} = T_Data, 
  
  Pages = pages_needed(T_Papers, U_Data),    

  Template_chosen = 
  if Pages > 2 ->
       case Count of
         "3" ->
           T_Papers;
         _ ->
           case Alt3 of
             "N/A" -> 
               io:format("No proper templates!~n"),
               halt();
             _ ->
               T_alt3 = parse_file(Alt3),
               {template, [{"alt2",_}, {"alt3",_},{"count",_}], T3_Papers} = deShell(T_alt3),           
               T3_Papers
           end           
       end;       
     Pages == 2 ->
       case Count of
         "1" ->
           case Alt2 of
             "N/A" -> 
               io:format("No proper templates!~n"),
               halt();
             _ ->
               T_alt2 = parse_file(Alt2),
               {template, [{"alt2",_}, {"alt3",_},{"count",_}], T2_Papers} = deShell(T_alt2),
               T2_Papers
           end;
         "2" ->
           T_Papers;
         "3" ->
           T_Papers
       end;       
     Pages == 1 ->
       T_Papers  
  end,      

  [F|FT] = Template_chosen,
  [M|MT] = FT,
  [E|[]] = MT,
  
  Template_info = #template_info{ counts = length(Template_chosen),
                                  page_amount_needed = Pages, 
                                  front_paper = if F == [] -> undefined; F =/= [] -> F end,
                                  middle_paper = if M == [] -> undefined; M =/= [] -> M end,
                                  end_paper = if E == [] -> undefined; E =/= [] -> E end
                                },
  
  Merged = merge_init(Template_info, U_Data),
  
  ParsedMergedData = parseMergedData(Merged),
  
  %%io:format("~p~n", [ParsedMergedData]).
  generatePDF(ParsedMergedData).
 

merge_init(Template_info, U_Data) ->
  case Template_info#template_info.counts of
    1 ->
      Template_info#template_info{front_paper = merge_init_aux(Template_info#template_info.front_paper, U_Data)};
    2 ->
      Template_info#template_info{front_paper = merge_init_aux(Template_info#template_info.front_paper, U_Data),
                                  end_paper = merge_init_aux(Template_info#template_info.end_paper, U_Data)};
    3 ->
      Template_info#template_info{front_paper = merge_init_aux(Template_info#template_info.front_paper, U_Data),
                                  middle_paper = merge_init_aux(Template_info#template_info.middle_paper, U_Data),
                                  end_paper = merge_init_aux(Template_info#template_info.end_paper, U_Data)}
  end.

merge_init_aux({paper, Attr, Paper}, U_Data) ->
  {paper, Attr, merge(Paper, U_Data)}.

%%%%%%%%%%%%%% Auxiliary functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deShell([{pi,_},{xml,Data}]) -> 
  case Data of
    {user, [], U_Data} ->
      parse_user_data(U_Data);
    _ ->
      Data
  end.

pages_needed(_Papers, _UserData) ->            %% modify later
  5.
  

parse_user_data_aux([], Acc) ->
  lists:reverse(Acc);
parse_user_data_aux([H|T], Acc) ->
  case H of
    {Key, _, [{_, Value}]} -> parse_user_data_aux(T, [{Key, Value}|Acc])
  end.
parse_user_data(Data) ->
  parse_user_data_aux(Data, []).

parseMergedData_for_each_frame({frame,  [{"bg",Bg},
					  {"break",Break},
					  {"class",Class},
					  {"continue",Continue},
					  {"font",Font},
					  {"fontsize",FontSize},
					  {"grid",Grid},
					  {"height",Height},
					  {"maxlines",MaxLines},
					  {"name",Name},
					  {"paraIndent",ParaIndent},
					  {"width",Width},
					  {"x",X},
					  {"y",Y}], [{raw, Content}]}) ->  
  Frame_info =   #frame_info{name = Name, 
		             class = Class,
		             x = list_to_integer(X),
		             y = 842 - list_to_integer(Y),
		             width = list_to_integer(Width),
		             height = list_to_integer(Height),
		             grid = to_bool(Grid),
		             bg = Bg,
		             font = Font,
		             fontsize = FontSize,
		             paraIndent = case ParaIndent of
                               "N/A" -> "N/A";
                               _ -> parse_paraIndent(ParaIndent)
                             end,
		             maxlines = case MaxLines of
                               "N/A" -> "N/A";
                               _ -> list_to_integer(MaxLines)
                             end,
		             continue = [Continue],
		             break = case Break of
                               "N/A" -> "N/A";
                               _ -> to_bool(Break)
                             end},
  %%io:format("~p~n", [Frame_info#frame_info.font]),
  {Frame_info, Content}.

parseMergedData_for_each_paper([], Acc) ->
  lists:reverse(Acc);
parseMergedData_for_each_paper([H|T], Acc) ->
  parseMergedData_for_each_paper(T, [parseMergedData_for_each_frame(H)|Acc]).

parseMergedData(Merged) ->
  case Merged#template_info.counts of
    1 ->
      Merged#template_info{front_paper = parse_aux(Merged#template_info.front_paper)};
    2 ->
      Merged#template_info{front_paper = parse_aux(Merged#template_info.front_paper),
                                  end_paper = parse_aux(Merged#template_info.end_paper)};
    3 ->
      Merged#template_info{front_paper = parse_aux(Merged#template_info.front_paper),
                                  middle_paper = parse_aux(Merged#template_info.middle_paper),
                                  end_paper = parse_aux(Merged#template_info.end_paper)}
  end.

parse_aux({paper, Attr, Paper}) ->
  {paper, Attr, parseMergedData_for_each_paper(Paper, [])}.
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

merge_aux([], _, Acc) ->
  Acc;
merge_aux([H|T], UserData, Acc) ->
  case insertData(UserData, H) of
    error -> 
      io:format("The template doesn't fit the user data table, please check if you are using the proper template!~n", []),
      erlang:halt();
    Inserted -> 
      merge_aux(T, UserData, Acc ++ [Inserted])    
  end.
merge(Template, UserData) ->
  merge_aux(Template, UserData, []).

%%%%%%%%%%%%%%%%%%%%%insertData function mainly use regular expression to perform the substitution of dynamic fields, test it before you run the whole system

%%%%%%%%%%%%%%%%%%%%%Successful!!!!!!!!!!!!!!!!
insertData(UserData, {frame, Arg, [{raw, Content}]}) ->
  case re:run(Content, "^@[a-zA-Z0-9_!@#$%^&*()]+") of
    {match, [{_, _}]} ->
          {frame, Arg, [{raw, Content}]};
    nomatch ->
	  case lists:keysearch("class", 1, Arg) of
	    {value, {"class", "table"}} ->
	       TagName = case lists:keysearch("name", 1, Arg) of
		 {value, {"name", Name}} -> Name;
		 false -> 
		   io:format("Unknow internal error in insertData function, search class~n"),
		   erlang:halt()      
	       end,
	       %%io:format("~p~n~p~n", [TagName, UserData]), %% debug
	       TableData = case lists:keysearch(list_to_atom(TagName), 1, UserData) of
		 {value, {_, TData}} -> TData;
		 false ->
		   io:format("Unknow internal error in insertData function, search ~p~n", [TagName]),
		   erlang:halt()  
	       end,
	       %%io:format("~p~n", [TableData]),  %%debug
	       MergedTableFormatAndData = Content ++ "@" ++ TableData,
	       %%io:format("~p~n",[MergedTableFormatAndData]),  %% debug
	       {frame, Arg, [{raw, MergedTableFormatAndData}]};
	    {value, {"class", _}} ->
		  case re:run(Content, "#[a-zA-Z0-9_]+", [global]) of
		    {match, MatchList} ->
		      case re:run(Content, "##[a-zA-Z0-9_]+", [global]) of
			{match, MatchList_filter} ->
			  M1 = lists:flatten(MatchList),
			  M2 = lists:flatten(MatchList_filter),
			  M2_mod = lists:map(fun({S, L})-> {S + 1, L - 1} end, M2),
			  MatchList_final = lists:subtract(M1, M2_mod);
			nomatch ->
			  MatchList_final = lists:flatten(MatchList)
		      end,
		      %io:format("~p~n", [MatchList_final]),  %%debug
		      case replaceData(UserData, MatchList_final, Content) of
			error -> error;
			ReplacedString -> 
			  %%remove slashes if strings like ##name exist in the content
			  {frame, Arg, [{raw, re:replace(ReplacedString, "##", "#", [global, {return, list}])}]}
		      end;
		    nomatch ->     
		      %%remove slashes if strings like ##name exist in the content
		      {frame, Arg, [{raw, re:replace(Content, "##", "#", [global, {return, list}])}]}
		  end;    
	    false ->
	      io:format("Unknow internal error in insertData function~n"),
	      erlang:halt()
	  end
  end.

replaceData(_, [], Content) ->
  Content;  
replaceData(UserData, [{Start, Len}|T], Content) ->
  %% +1 because re module counts from 0 and lists module counts from 1; +1 again to skip # mark
  Param = list_to_atom(lists:sublist(Content, Start + 1 + 1, Len - 1)),  
  case lists:keysearch(Param, 1, UserData) of
    {value, {_, Value}} -> 
      %%Update the position of matches in the match list
      T_mod = lists:map(fun({S, L})-> {S + length(Value) - Len, L} end, T),      
      replaceData(UserData, T_mod, re:replace(Content, "#[a-zA-Z0-9_]+", Value, [{return, list}, {offset, Start}]));  %% offset indicates the right token to be replaced 
    false -> error
  end.

%%%%%%%%%%%%%%%%%%%%%%%%Generate PDF%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generatePDF_for_each_page([], _, _) ->
  ok;
generatePDF_for_each_page([{Frame_info, Content}|T], Merged, PDF) ->
  case Frame_info#frame_info.class of
    "text" ->
      text_img_table_list:parse_text(Frame_info, Content, Merged, PDF);  
    "img" ->
      text_img_table_list:parse_img(Frame_info, Content, Merged, PDF);  
    "table" ->
      text_img_table_list:parse_table(Frame_info, Content, Merged, PDF);  
    "list" ->
      text_img_table_list:parse_list(Frame_info, Content, Merged, PDF);  
    false ->
      io:format("Template error!~n"),
      halt() 
  end,  
  
  generatePDF_for_each_page(T, Merged, PDF).

generatePDF_for_pages([], _) ->
  ok;
generatePDF_for_pages(Merged, PDF) ->
  {paper, _, H} = Merged#template_info.front_paper,
  generatePDF_for_each_page(H, Merged, PDF).

generatePDF(Merged) ->
  PDF = eg_pdf:new(),
  eg_pdf:set_pagesize(PDF,a4),
  eg_pdf:set_author(PDF,"Klarna"),
  eg_pdf:set_title(PDF, "Transpromo"),
  eg_pdf:set_subject(PDF,"Mailmerging project"),
  eg_pdf:set_keywords(PDF,"Erlang, PDF, Gutenberg, Klarna"),
  {Y, M, D} = date(), 
  eg_pdf:set_date(PDF,Y, M, D),

  generatePDF_for_pages(Merged, PDF),

  Serialised = eg_pdf:export(PDF),
  file:write_file("Klarna Invoice With Transpromo.pdf",[Serialised]),
  eg_pdf:delete(PDF).

