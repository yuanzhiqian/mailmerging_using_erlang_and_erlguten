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

-import(erlguten, [parse_fontSize/1]).

main(ArgList) ->
  [Template, UserData] =  ArgList, 
  T = erlguten_xml_lite:parse_file(Template),
  U = erlguten_xml_lite:parse_file(UserData),
  
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
               T_alt3 = erlguten_xml_lite:parse_file(Alt3),
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
               T_alt2 = erlguten_xml_lite:parse_file(Alt2),
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
  Merged = merge_init(Template_chosen, U_Data, []),
  generatePDF(Merged).
  %%io:format("~p~n", [Merged]).  %%debug

merge_init([], _, Acc) ->
  lists:reverse(Acc);
merge_init([{paper,_,Paper}|T], UserData, Acc) ->
  merge_init(T, UserData, [merge(Paper, UserData)|Acc]).

%%%%%%%%%%%%%% Auxiliary functions %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

deShell([{pi,_},{xml,Data}]) -> 
  case Data of
    {user, [], U_Data} ->
      parse_user_data(U_Data);
    _ ->
      Data
  end.

pages_needed(_Papers, _UserData) ->            %% modify later
  3.
  

parse_user_data_aux([], Acc) ->
  lists:reverse(Acc);
parse_user_data_aux([H|T], Acc) ->
  case H of
    {Key, _, [{_, Value}]} -> parse_user_data_aux(T, [{Key, Value}|Acc])
  end.
parse_user_data(Data) ->
  parse_user_data_aux(Data, []).

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
           io:format("Unknow internal error in insertData function, search TagName~n"),
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

%%%%%%%%%%%%%%%%%%%%%%%%Generate PDF

generatePDF_for_each_frame([], _) ->
  ok;
generatePDF_for_each_frame([{frame, Attr, [{raw, Content}]}|T], PDF) ->
  X = 
  case lists:keysearch("x", 1, Attr) of
    {value, {_, X_Value}} ->
      list_to_integer(X_Value);
    false ->
      io:format("Template error!~n"),
      halt()  
  end,

  Y = 842 -
  case lists:keysearch("y", 1, Attr) of
    {value, {_, Y_Value}} ->
      list_to_integer(Y_Value);
    false ->
      io:format("Template error!~n"),
      halt()  
  end,

  Font = 
  case lists:keysearch("font", 1, Attr) of
    {value, {_, "N/A"}} -> "Times-Roman";  %% temprarily
    {value, {_, Font_Value}} ->
      Font_Value;
    false ->
      io:format("Template error!~n"),
      halt()  
  end,

  {Pt, Leading} = 
  case lists:keysearch("fontsize", 1, Attr) of
    {value, {_, "N/A"}} -> parse_fontSize("12/24");  %% temprarily
    {value, {_, FontSize_Value}} ->
      parse_fontSize(FontSize_Value);
    false ->
      io:format("Template error!~n"),
      halt()  
  end,

  Text = Content,

  pdf:save_state(PDF),
  pdf:begin_text(PDF),
  pdf:set_font(PDF,Font, Pt),
  pdf:set_text_rendering(PDF, fill),
  pdf:set_text_pos(PDF, X, Y),  
  pdf:textbr(PDF, Text),
  pdf:end_text(PDF),
  pdf:restore_state(PDF),
  
  generatePDF_for_each_frame(T, PDF).

generatePDF_for_each_page([], _) ->
  ok;
generatePDF_for_each_page([H|T], PDF) ->
  generatePDF_for_each_frame(H, PDF),
  pdf:new_page(PDF),
  generatePDF_for_each_page(T, PDF).  

generatePDF(Merged) ->
  PDF = pdf:new(),
  pdf:set_pagesize(PDF,a4),
  pdf:set_author(PDF,"Klarna"),
  pdf:set_title(PDF, "Transpromo"),
  pdf:set_subject(PDF,"Mailmerging project"),
  pdf:set_keywords(PDF,"Erlang, PDF, Gutenberg, Klarna"),
  pdf:set_date(PDF,2011,4,7),

  generatePDF_for_each_page(Merged, PDF),

  Serialised = pdf:export(PDF),
  file:write_file("Klarna Invoice With Transpromo.pdf",[Serialised]),
  pdf:delete(PDF).

