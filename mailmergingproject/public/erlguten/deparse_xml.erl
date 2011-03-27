%%================================================
%% Deparse the erlang tokens into xml file
%%------------------------------------------------
%% Author: Yuan Zhiqian
%%
%%================================================

-module(deparse_xml).
-compile(export_all).

test() ->
  Exp = [{pi,"xml version=\"1.0\" "},
 {xml,{user,[{"number", "one"}],
            [{name,[{"gender", "male"}],[{raw,"John"}]},{payment,[{"currency", "Kronor"}, {"due_date", "Monday"}],[{raw,"100Kr"}]}]}}],
  Xml = parse(Exp),
  file:write_file("test_parse.xml", [Xml]),
  io:format("~p~n", [Xml]).

main(E, File_name) ->
  Xml = parse(E),
  file:write_file(File_name, [Xml]),
  ok.

parse([{pi,"xml version=\"1.0\" "}, {xml, Tag}]) ->
  XmlHeader = "<?xml version=\"1.0\" ?>",
  XmlHeader ++ parseTag(Tag).
  
parseTag({Name, Attr, Content}) ->
  AttrStr = parseAttr(Attr, ""),
  %io:format("~p~n", [AttrStr]),
  ContentStr = parseContent(Content, ""),
  XmlStr = "<" ++ atom_to_list(Name) ++ AttrStr ++">" ++ ContentStr ++ "</" ++ atom_to_list(Name) ++ ">",
  XmlStr.

parseAttr([], Acc) ->
  Acc;
parseAttr([{Key, Value}|T], Acc) ->
  Quoted_Value = "\"" ++ Value ++ "\"",
  AttrStr = Key ++ "=" ++ Quoted_Value,
  parseAttr(T, Acc++" "++AttrStr).

parseContent([], Acc) ->
  Acc;
parseContent([H|T], Acc) ->
  case H of
    {raw, Value} -> parseContent(T, Acc ++ Value);
    Tag -> parseContent(T, Acc ++ parseTag(Tag))
  end.
