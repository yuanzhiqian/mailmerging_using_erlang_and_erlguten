%%================================================
%% mail merge
%%------------------------------------------------
%% Author: Yuan Zhiqian
%%
%%================================================

-module(mail_merge).
-compile(export_all).

test() ->
  merge_and_transform(["template.xml", "userdata.xml"]).  

test2() ->
  io:format("~p~n", [os:cmd("pwd")]).

merge_and_transform(ArgList) ->
  io:format("Begin merg_and_transform~n"),
  [Template, UserData] =  ArgList,
  io:format("~p~n~p~n",[Template, UserData]),
  F1 = erlguten_xml_lite:parse_file(Template),
  F2 = erlguten_xml_lite:parse_file(UserData),
  %io:format("~p~n", [F1]),
  Template_Data = deShell(F1),
  %io:format("~p~n", [Template_Data]),
  case F2 of 
    {error, W} ->
	    io:format("Error in source:~p~p~n",[F2, W]),
	    exit(1);
	[{pi,_},{xml,{user,_, Data}}] ->
            %io:format("~p~n",[Data]),
            UserDataMap = parse_user_data(Data, []),
            Merged = merge(UserDataMap, Template_Data),
            io:format("~p~n", [Merged]),
            Galley_name = "my_test_galley.gal",
            {Literal, Galley} = extract_and_transform(Merged, Galley_name),
            %io:format("~p~n", [Literal]),
            %io:format("~p~n", [Galley]),
            Data_name = "my_test.xml",
            deparse_xml:main(Literal, Data_name),
            deparse_xml:main(Galley, Galley_name),
            erlguten:batch([list_to_atom(Data_name)])
            %os:cmd("./erlguten " ++ Data_name)                      
  end,  
  ok.

deShell([{pi,_},{xml,{paper,_, Data}}]) -> Data.

parse_user_data([], Acc) ->
  %io:format("~p~n", [lists:reverse(Acc)]),
  lists:reverse(Acc);
parse_user_data([H|T], Acc) ->
  case H of
    {Key, _, [{_, Value}]} -> parse_user_data(T, [{Key, Value}|Acc])
  end.

merge_aux(_, [], Acc) ->
  Acc;
merge_aux(UserData, [H|T], Acc) ->
  io:format("~p~n",[H]),
  case insertData(UserData, H) of
    error -> io:format("The template doesn't fit the user data table, please check if you are using the proper template!");
    Inserted -> merge_aux(UserData, T, Acc ++ [Inserted])    
  end.
merge(UserData, Template) ->
  merge_aux(UserData, Template, []).

%%%%%%%%%%%%%%%%%%%%%insertData function mainly use regular expression to perform the substitution of dynamic fields, test it before you run the whole system

%%%%%%%%%%%%%%%%%%%%%Successful!!!!!!!!!!!!!!!!
test_insert_data()->
  case insertData([{name, "John"},{price, "150.00Kr"}, {nonexistfield, "N/A"}], {frame, arg, "Hello #name, you need to pay #price"}) of
    {frame, _, Content} -> io:format("~p~n", [Content])
  end.

insertData(UserData, {frame, Arg, [{raw, Content}]}) ->
  case re:run(Content, "#[a-zA-Z0-9_]*", [global]) of
    {match, MatchList} ->
      io:format("~p~n",[MatchList]),
      {frame, Arg, [{raw, replaceData(UserData, lists:flatten(MatchList), Content)}]};
    {error, _} ->
      io:format("There is an error in the regular expression! ~n", [])
  end.

replaceData_aux(_, [], _, NewContent) ->
  NewContent;
replaceData_aux(UserData, [{Start, Len}|T], Content, NewContent) ->
  %% +1 because re module counts from 0 and lists module counts from 1; +1 again to skip # mark
  Param = list_to_atom(lists:sublist(Content, Start + 1 + 1, Len - 1)),  
  io:format("~p~n", [Param]), 
  case lists:keysearch(Param, 1, UserData) of
    {value, {_, Value}} -> 
      replaceData_aux(UserData, T, Content, re:replace(NewContent, "#[a-zA-Z0-9_]*", Value, [{return, list}]));        
    false -> error
  end.
replaceData(UserData, MatchList, Content) ->
  replaceData_aux(UserData, MatchList, Content, Content).

%%%%%%%%%%%%%%%%%%%%%%%%%%%Above is the merge part%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%Functions below deal with the pdf generating part%%%%%%%%%%%%%%%
extract_and_transform(Merged, Galley_name) ->
  {Boxes, Galley} = first_parse(Merged, []),
  Literal = second_parse(Merged, Galley_name, Boxes, []),
  {Literal, Galley}.

first_parse([], Acc) -> 
  Galley = [{pi,"xml version=\"1.0\" "},
            {xml,{galley,[],
                         lists:reverse(Acc) }}],
  {lists:reverse(Acc), Galley};
first_parse([H|T], Acc) ->
  first_parse(T, [pack_box(H)|Acc]).

pack_box({frame, Attr, _}) ->
  BG = "default",
  Continue = "none",
  %io:format("~p~n", [Attr]),
  Fontsize = extract_from_attr(fontsize, Attr),
  Grid = "false",
  Lines = extract_from_attr(maxlines, Attr),
  Measure = "30",
  Name = extract_from_attr(name, Attr),
  X = extract_from_attr(x, Attr),
  YLiteral = extract_from_attr(y, Attr),
  Y = integer_to_list(842-list_to_integer(YLiteral)),

  Obj_name = "p",
  Obj_para_ind = "0",
  
  Tag_break = "true",
  Tag_font = extract_from_attr(font, Attr),
  Tag_name = "raw",

  Box = {box,[{"bg", BG},
              {"continue", Continue},
              {"fontSize", Fontsize}, 
              {"grid", Grid}, 
              {"lines", Lines},
              {"measure", Measure},
              {"name", Name},
              {"x", X},
              {"y", Y}],
             [{obj, [{"name", Obj_name},{"paraIndent", Obj_para_ind}], 
                    [{tag, [{"break", Tag_break},{"font", Tag_font},{"name", Tag_name}],
                   []}]}]},
  Box.
  
extract_from_attr(Atom, Attr) ->
  case lists:keysearch(atom_to_list(Atom), 1, Attr) of
    {value, {_, Value}} -> Value
  end.

second_parse([], _, _, Acc) -> 
  Literal = [{pi,"xml version=\"1.0\" "},
             {xml,{document,[],
                         lists:reverse(Acc) }}],
  Literal;
second_parse([H|T], Galley_name, [Hbox|Tbox], Acc) ->
  second_parse(T, Galley_name, Tbox, [pack_flow(H, Galley_name, Hbox)|Acc]).

pack_flow({frame, _, Content}, Galley_name, 
          {box,[_,_,_,_,_,_,{"name", Name},_,_],
             [{obj, [{"name", Obj_name},_], 
                    [{tag, [_,_,{"name", Tag_name}],
                   []}]}]} ) ->
  Flow = {flow, [{"galley", Galley_name}, {"name", Name}], [{list_to_atom(Obj_name), [], [{list_to_atom(Tag_name), [], Content}]}]},
  Flow.
  

  
