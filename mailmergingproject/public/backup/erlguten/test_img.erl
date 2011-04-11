-module(test_img).

-compile(export_all).

test() ->

  PDF = pdf:new(),
  pdf:set_pagesize(PDF,a4),
  pdf:set_author(PDF,"Klarna"),
  pdf:set_title(PDF, "Transpromo"),
  pdf:set_subject(PDF,"Mailmerging project"),
  pdf:set_keywords(PDF,"Erlang, PDF, Gutenberg, Klarna"),
  {Y, M, D} = date(), 
  pdf:set_date(PDF,Y, M, D),

  pdf:image(PDF,"img/slip.jpg",{390,440},{height,140}),
 
  Serialised = pdf:export(PDF),
  io:format("~p~n", [Serialised]),
  file:write_file("test img.pdf",[Serialised]),
  pdf:delete(PDF),
  ok.
