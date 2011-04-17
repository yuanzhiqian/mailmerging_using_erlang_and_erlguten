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

-import(functions_from_old_erlguten, [parse_fontSize/1, to_bool/1, parse_paraIndent/1]).

parameters(Dict) ->
  receive
    {append, {Key, Value}} ->
      parameters(dict:append(Key, Value, Dict));
    {retrieve, Key, Pid} ->
      case dict:find(Key, Dict) of
        {ok, Value} -> Pid ! {ok, Value};
        error -> Pid ! {error, parameterNotFound}
      end,      
      parameters(Dict);
    {stop} ->
      done
  end.

startPara() -> 
  Pid = spawn(mail_merge, parameters, [dict:new()]),
  register(para, Pid).

storePara({Key, Value}) ->
  para ! {append, {Key, Value}}.

getPara(Key) ->
  para ! {retrieve, Key, self()},
  Return =
  receive
    {ok, [Value]} -> Value;
    {error, Reason} ->
      io:format("Error:", [Reason]),
      halt()
  end,
  Return.

stopPara() ->
  para ! {stop}.

main(ArgList) ->
  startPara(),
  [Dir, Template, UserData] =  ArgList, 
  storePara({dir, Dir}),
  T = eg_xml_lite:parse_file(atom_to_list(Dir) ++ atom_to_list(Template)),
  U = eg_xml_lite:parse_file(atom_to_list(Dir) ++ atom_to_list(UserData)),
  
  T_Data = deShell(T),  
  U_Data = deShell(U),
  
  %% The count indicates the number of papers defined, i.e. whether all front, middle and end are defined or only some of them are defined. And the alt2 ,alt3 
  %%     shows the template to use if current template can't hold all the contents
  {template, [{"alt2",Alt2}, {"alt3",Alt3},{"count",Count}], T_Papers} = T_Data, 
  
  {Pages, Template_chosen} = preprocess(Count, atom_to_list(Dir) ++ Alt2, atom_to_list(Dir) ++ Alt3, T_Papers, U_Data),
  %%io:format("Pages:~p~n", [Pages]),
  
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
  
  %%io:format("~p~n", [ParsedMergedData#template_info.page_amount_needed]),
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


%%%%%%%%%% In prepocessing, the y axis is 842 - y1, in which y1 refers to the coordination in pdf, i.e. the y field in Frame_info and erlguten context
preprocess(Count, Alt2, Alt3, T_Papers, U_Data) ->
  TemplateChosen = 
  case Count of
    "3" ->
      T_Papers;
    "2" ->
      case overThreshold2(T_Papers, U_Data) of
        true -> 
          T3_Papers = 
          case Alt3 of
             "N/A" -> 
               io:format("No proper templates!~n"),
               halt();
             _ ->
               T_alt3 = eg_xml_lite:parse_file(Alt3),
               {template, [{"alt2",_}, {"alt3",_},{"count",_}], T3_PapersFound} = deShell(T_alt3),           
               T3_PapersFound
           end,
           T3_Papers;
        false ->
          T_Papers
      end;
    "1" ->
      case overThreshold1(T_Papers, U_Data) of
        true ->
          T2_Papers = 
          case Alt2 of
             "N/A" -> 
               io:format("No proper templates!~n"),
               halt();
             _ ->
               T_alt2 = eg_xml_lite:parse_file(Alt2),
               {template, [{"alt2",_}, {"alt3",_},{"count",_}], T2_PapersFound} = deShell(T_alt2),
               T2_PapersFound
          end,
          case overThreshold2(T2_Papers, U_Data) of
		true -> 
                  %%io:format("debug~n"),
                  T3_Papers =
		  case Alt3 of
		     "N/A" -> 
		       io:format("No proper templates!~n"),
		       halt();
		     _ ->
		       T_alt3 = eg_xml_lite:parse_file(Alt3),
		       {template, [{"alt2",_}, {"alt3",_},{"count",_}], T3_PapersFound} = deShell(T_alt3),           
		       T3_PapersFound
		  end,
                  T3_Papers;
		false ->
		  T2_Papers
          end;
        false ->
          T_Papers
      end
  end,
  %%io:format("~p~n", [TemplateChosen]),
  {pages_needed(length(TemplateChosen), TemplateChosen, U_Data), TemplateChosen}.

%%%%%%%%%%auxilary
tableBottom(TableArg, Format, UserData) -> 
  TagName = case lists:keysearch("name", 1, TableArg) of
		 {value, {"name", Name}} -> Name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

  Y = case lists:keysearch("y", 1, TableArg) of
        {value, {_, YData}} -> list_to_integer(YData);
        false ->
		   io:format("Unknow internal error!~n"),
		   erlang:halt() 
      end,
  {_, Leading} = case lists:keysearch("fontsize", 1, TableArg) of
        {value, {_, FontSize}} -> 
                   parse_fontSize(FontSize);
        false ->
		   io:format("Unknow internal error!~n"),
		   erlang:halt() 
                  end,
  
  [_Columns, Headers] = text_img_table_list:parseFormat(Format),

  TableData = case lists:keysearch(list_to_atom(TagName), 1, UserData) of
		 {value, {_, TData}} -> TData;
		 false ->
		   io:format("Unknow internal error!~n"),
		   erlang:halt()  
	      end,
  RowsInList = text_img_table_list:parseTableContent(TableData),
  TableLength = case Headers of
                  [] -> length(RowsInList);
                  _ -> length(RowsInList) + 1
                end,
  TableBottom = Y + TableLength * Leading,
  TableBottom. 


bottomBoundary_aux(_, [], BottomBoundary) ->
  BottomBoundary;
bottomBoundary_aux({TagName,X,Y,Width}, [{frame, Arg, _Content}|T], BottomBoundary) ->
  X1 = case lists:keysearch("x", 1, Arg) of
	  {value, {_, XData}} -> list_to_integer(XData);
	  false ->
		io:format("Unknow internal error!~n"),
		erlang:halt() 
       end,
  Y1 = case lists:keysearch("y", 1, Arg) of
	  {value, {_, YData}} -> list_to_integer(YData);
	  false ->
		io:format("Unknow internal error!~n"),
		erlang:halt() 
       end,

  if Y1 < Y -> bottomBoundary_aux({TagName,X,Y,Width}, T, BottomBoundary);
     Y1 >=Y ->
       case lists:keysearch("name", 1, Arg) of
         {value, {_, Name}} ->
             if TagName == Name -> 
                  %%io:format("found itself~n"),
                  %%io:format("name: ~p~n~p~n", [TagName, Name]),
                  bottomBoundary_aux({TagName,X,Y,Width}, T, BottomBoundary);
                TagName =/= Name -> 
                       %%io:format("name: ~p~n~p~n", [TagName, Name]),
                       if 
			 X1 > X + Width -> bottomBoundary_aux({TagName,X,Y,Width}, T, BottomBoundary);
			 X1 =< X + Width -> 
                           if Y1 < BottomBoundary -> bottomBoundary_aux({TagName,X,Y,Width}, T, Y1);
                              Y1 >= BottomBoundary -> bottomBoundary_aux({TagName,X,Y,Width}, T, BottomBoundary)
                           end
		       end
             end;
         false ->
             io:format("Unknow internal error!~n"),
	     erlang:halt() 
       end       
  end.

bottomBoundary({TagName,X,Y,Width}, FramesInThePage) ->
  %%io:format("paper length~n~p~n", [length(FramesInThePage)]),
  bottomBoundary_aux({TagName,X,Y,Width}, FramesInThePage, 842).

findContinueTablePosition(_, []) ->                          %% in this case, just "fake" one
  {{frame, [], [raw, ""]},0,0}; 
findContinueTablePosition(TagName, [{frame, Arg, Content}|T]) ->
  case lists:keysearch("name", 1, Arg) of
         {value, {_, Name}} ->
             if TagName == Name -> 
                  
                  X = case lists:keysearch("x", 1, Arg) of
			{value, {_, XData}} -> list_to_integer(XData);
			false ->
				   io:format("Unknow internal error!~n"),
				   erlang:halt() 
		      end,
		  Y = case lists:keysearch("y", 1, Arg) of
			{value, {_, YData}} -> list_to_integer(YData);
			false ->
				   io:format("Unknow internal error!~n"),
				   erlang:halt() 
		      end,

                  {{frame, Arg, Content}, X, Y};
                TagName =/= Name -> 
                  findContinueTablePosition(TagName, T)
             end;
         false ->
             io:format("Unknow internal error!~n"),
	     erlang:halt() 
  end.

%%%%%%%////auxilary end

overThreshold1([{paper, _Arg, Frames}], U_Data) ->
  overThreshold1_aux(Frames, U_Data, Frames).

overThreshold1_aux([], _, _) ->
  false;
overThreshold1_aux([{frame, Arg, Content}|T], UserData, Frames) ->
  case lists:keysearch("class", 1, Arg) of
    {value, {"class", "table"}} ->      
          [{raw, Format}] = Content,
          TableBottom = tableBottom(Arg, Format, UserData),

          TagName = case lists:keysearch("name", 1, Arg) of
		 {value, {"name", Name}} -> Name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

	  X = case lists:keysearch("x", 1, Arg) of
		{value, {_, XData}} -> list_to_integer(XData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Y = case lists:keysearch("y", 1, Arg) of
		{value, {_, YData}} -> list_to_integer(YData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Width = case lists:keysearch("width", 1, Arg) of
		{value, {_, WidthData}} -> list_to_integer(WidthData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
		  end,

          BottomLimit = bottomBoundary({TagName, X, Y, Width}, Frames),
  
          %%io:format("~p~n~p~n", [TableBottom, BottomLimit]), %% debug
    
          if TableBottom >= BottomLimit -> true;
             TableBottom < BottomLimit -> overThreshold1_aux(T, UserData, Frames)
          end;
    {value, {"class", _}} -> overThreshold1_aux(T, UserData, Frames);
    false ->
	      io:format("Unknow internal error!~n"),
	      erlang:halt()
  end.

overThreshold2_aux([], _, _) ->
  false;
overThreshold2_aux([{frame, Arg, Content}|T], UserData, {Frames1, Frames2}) ->
  %%io:format("over2_aux~n~p~n", [Frames2]),
  case lists:keysearch("class", 1, Arg) of
    {value, {"class", "table"}} ->      
          [{raw, Format}] = Content,
          TableBottom = tableBottom(Arg, Format, UserData),

          TagName = case lists:keysearch("name", 1, Arg) of
		 {value, {"name", Name}} -> Name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

          X = case lists:keysearch("x", 1, Arg) of
		{value, {_, XData}} -> list_to_integer(XData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Y = case lists:keysearch("y", 1, Arg) of
		{value, {_, YData}} -> list_to_integer(YData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Width = case lists:keysearch("width", 1, Arg) of
		{value, {_, WidthData}} -> list_to_integer(WidthData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
		  end,
          
          BottomLimit1 = bottomBoundary({TagName, X, Y, Width}, Frames1),
          ContinueTableName = case lists:keysearch("continue", 1, Arg) of
		 {value, {"continue", Con_name}} -> Con_name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

          {_Con_frame, _X_con, Y_con} = findContinueTablePosition(ContinueTableName, Frames2),

          BottomLimit2 = bottomBoundary({ContinueTableName, X, Y_con, Width}, Frames2),

          %%io:format("~p~n~p~n~p~n~p~n", [Y_con, TableBottom, BottomLimit1, BottomLimit2]),

          if 
             TableBottom < BottomLimit1 -> overThreshold2_aux(T, UserData, {Frames1, Frames2});
             TableBottom >= BottomLimit1 andalso (TableBottom + Y_con - BottomLimit1) < BottomLimit2 -> overThreshold2_aux(T, UserData, {Frames1, Frames2});
             TableBottom >= BottomLimit1 andalso (TableBottom + Y_con - BottomLimit1) >= BottomLimit2 -> true
          end;
    {value, {"class", _}} -> overThreshold2_aux(T, UserData, {Frames1, Frames2});
    false ->
	      io:format("Unknow internal error!~n"),
	      erlang:halt()
  end.

overThreshold2([{paper, _Arg1, Frames1}|[{paper, _Arg2, Frames2}]], U_Data) ->
  %% Suppose the table only starts from the first page -- Ask Erik later
  overThreshold2_aux(Frames1, U_Data, {Frames1, Frames2}).                                                               

pages_needed(Count, TemplateChosen, U_Data) ->
  case Count of
    1 ->
      1;
    2 ->
      2;
    3 ->
      countPages(TemplateChosen, U_Data, 3)
  end.

countPages([{paper, _Arg1, Frames1}, {paper, _Arg2, Frames2}, {paper, _Arg3, Frames3}], U_Data, MaxPages) ->
  %% Suppose the table only starts from the first page -- Ask Erik later  
  countPages_aux(Frames1, U_Data,{Frames1, Frames2, Frames3}, MaxPages).

countPages_aux([], _, _, MaxPages) ->
  MaxPages;
countPages_aux([{frame, Arg, Content}|T], UserData,{Frames1, Frames2, Frames3}, MaxPages) ->
   case lists:keysearch("class", 1, Arg) of
    {value, {"class", "table"}} ->      
          [{raw, Format}] = Content,
          TableBottom = tableBottom(Arg, Format, UserData),

          TagName = case lists:keysearch("name", 1, Arg) of
		 {value, {"name", Name}} -> Name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

          X = case lists:keysearch("x", 1, Arg) of
		{value, {_, XData}} -> list_to_integer(XData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Y = case lists:keysearch("y", 1, Arg) of
		{value, {_, YData}} -> list_to_integer(YData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
	      end,
	  Width = case lists:keysearch("width", 1, Arg) of
		{value, {_, WidthData}} -> list_to_integer(WidthData);
		false ->
			   io:format("Unknow internal error!~n"),
			   erlang:halt() 
		  end,

          %% Don't forget to count the starting position of table in middle and end pages, it is not always zero

          BottomLimit1 = bottomBoundary({TagName,X,Y,Width}, Frames1),
          %% Suppose the middle pages don't have slips below table

          ContinueTableName = case lists:keysearch("continue", 1, Arg) of
		 {value, {"continue", Con_name}} -> Con_name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

          {{frame, Arg_con, _}, _X_con1, Y_con1} = findContinueTablePosition(ContinueTableName, Frames2),

          NextContinueTableName = case lists:keysearch("continue", 1, Arg_con) of
		 {value, {"continue", Next_Con_name}} -> Next_Con_name;
		 false -> 
		   io:format("Unknow internal error!~n"),
		   erlang:halt()      
	       end,

          {_Con_frame2, _X_con2, Y_con2} = findContinueTablePosition(NextContinueTableName, Frames3),          
          
          BottomLimit2 = bottomBoundary({NextContinueTableName,X,Y_con2,Width}, Frames3),

          %%io:format("~p~n~p~n~p~n", [TableBottom, BottomLimit1, BottomLimit2]),

          %%%%%%%      (height of front page slip    +    height of end page slip   +    (table y + table height) ) / page height
          NewMaxPages_Temp = ceiling(((842 - BottomLimit1) + (842 - BottomLimit2) + TableBottom) / 842),  
          %%%%%% Let me explain a bit for the algorithm of calculating RealTableBottom:
          %%%%%%           oldTableBot +   middle pages * start position +  end page start position
          NewTableBottom = TableBottom + (NewMaxPages_Temp - 2) * Y_con1  + Y_con2,

          %%io:format("realmaxpages: ~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n", [Y, Y_con1, Y_con2, BottomLimit1, BottomLimit2, NewTableBottom, NewMaxPages_Temp]),          
          RealMaxPages =  realMaxPages(Y, Y_con1, Y_con2, BottomLimit1, BottomLimit2, NewTableBottom, NewMaxPages_Temp),

          MaxPagesUpdated = 
          if RealMaxPages > MaxPages -> RealMaxPages;
             RealMaxPages =< MaxPages -> MaxPages
          end,
          countPages_aux(T, UserData, {Frames1, Frames2, Frames3}, MaxPagesUpdated);        
    {value, {"class", _}} -> countPages_aux(T, UserData, {Frames1, Frames2, Frames3}, MaxPages);
    false ->
	      io:format("Unknow internal error!~n"),
	      erlang:halt()
  end.

realMaxPages(Y, Y_con1, Y_con2, BottomLimit1, BottomLimit2, TableBottom, MaxPages) ->
  
  NewMaxPages = ceiling(((842 - BottomLimit1) + (842 - BottomLimit2) + TableBottom) / 842),
  
  if      
     NewMaxPages > MaxPages -> 
       %%%                 extra "heading" frames(e.g. trademark) for middle pages
       NewTableBottom = (NewMaxPages - MaxPages) * Y_con1 + TableBottom,
       %%io:format("realmaxpages: ~n~p~n~p~n~p~n~p~n~p~n~p~n~p~n", [Y, Y_con1, Y_con2, BottomLimit1, BottomLimit2, NewTableBottom, NewMaxPages]),
       realMaxPages(Y, Y_con1, Y_con2, BottomLimit1, BottomLimit2, NewTableBottom, NewMaxPages);
     NewMaxPages =< MaxPages -> 
       %%io:format("ever happens?~n"),
       MaxPages         
  end.
   

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.

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
		   io:format("Unknow internal error in insertData function!~n"),
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

  {Serialised, _PageNo} = eg_pdf:export(PDF),
  file:write_file("Klarna Invoice With Transpromo.pdf",[Serialised]),
  eg_pdf:delete(PDF),
  stopPara().

