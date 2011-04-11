-module(from_old_erlguten).

-compile(export_all).

-import(lists, [foreach/2, map/2]).

%%%%%%%%%%%%pdf module




%%%%%%%%%%%erlguten module
parse_fontSize(S) ->
    case string:tokens(S, "/") of
	[A, B] ->
	    I = parse_int("fontSize",  A),
	    J = parse_int("fontSize",  B),
	    {I, J};
	_ ->
	    io:format("fontSize must be of the form <int>/<int> was:~s",[S]),
	    exit(1)
    end.


parse_paraIndent(S) ->
    case string:tokens(S, ",") of
	Toks ->
	    map(fun(I) -> parse_int("paraIndent",  I) end, Toks);
	_ ->
	    io:format("paraIndent must be of the form <int>,<int>, was:~s",
		      [S]),
	    exit(1)
    end.

parse_int(Txt, S) ->
    case (catch list_to_integer(S)) of
	{'EXIT', _} ->
	    io:format("invalid integer:~s expecting:~s~n", [S, Txt]),
	    exit(1);
	I ->
	    I
    end.

parse_float(S) ->
    case (catch list_to_float(S)) of
	{'EXIT', _} ->
	    io:format("invalid float:~s ~n", [S]),
	    exit(1);
	I ->
	    I
    end.

parse_color("default") -> no;
parse_color(Str) ->
    [R,G,B] = string:tokens(Str,","),
    {yes, {parse_float(R), parse_float(G), parse_float(B)}}.


to_bool("true") ->
    true;
to_bool("false") ->
    false;
to_bool(X) ->
    io:format("expecting true or false got:~s~n",[X]),
    exit(1).

zip1([H1|T1],[H2|T2]) -> [[H1," ",H2]|zip1(T1, T2)];
zip1([], [])          -> [].


