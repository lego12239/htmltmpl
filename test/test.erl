-module(test).
-export([start/0]).

-record(test, {id, title, data}).


make_html(File, Data) ->
    Res = htmltmpl:open(File),
    case Res of
	{ok, Tmpl} ->
	    htmltmpl:apply(Tmpl, Data);
	_ ->
	    throw(Res)
    end.


read_master_html(File) ->
    Res = file:read_file(File),
    case Res of
	{ok, Output} ->
	    binary_to_list(Output);
	_ ->
	    throw(Res)
    end.


do_test([], Len, _Num, Passed_num) ->
    io:format("~nTotal: ~w/~w~n", [Passed_num, Len]);

do_test([H|T], Len, Num, Passed_num) ->
    io:format("~w/~w: ~s  ", [Num, Len, H#test.title]),
    Result = read_master_html(H#test.id ++ ".html"),
    Output = make_html(H#test.id ++ ".tmpl", H#test.data),
    case Result =:= Output of
	true ->
	    io:format("ok~n"),
	    Passed_num_new = Passed_num + 1;
	false ->
	    io:format("fail~n"),
	    Passed_num_new = Passed_num
    end,
    do_test(T, Len, Num + 1, Passed_num_new).

start() ->
    T = [#test{id="01",
	       title="Test TMPL_VAR",
	       data=[{"title", "There is a title here"}]},
	 #test{id="02",
	       title="Test TMPL_VAR with no data",
	       data=[]},
	 #test{id="03",
	       title="Test TMPL_LOOP",
	       data=[{"loop1", [[{"v1", "1"}],
				[{"v1", "2"}]]}]}],
    do_test(T, length(T), 1, 0).
