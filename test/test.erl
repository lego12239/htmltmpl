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


do_test([], _Num, Passed_num, _F) ->
    Passed_num;

do_test([H|T], Num, Passed_num, F_report_start) ->
    F_report_start(Num, H#test.title),
    Result = read_master_html(H#test.id ++ ".html"),
    Output = make_html(H#test.id ++ ".tmpl", H#test.data),
    case Result =:= Output of
	true ->
	    report_status("ok"),
	    Passed_num_new = Passed_num + 1;
	false ->
	    report_status("fail"),
	    Passed_num_new = Passed_num
    end,
    do_test(T, Num + 1, Passed_num_new, F_report_start).


make_report_fun(T) ->
    Len = length(T),
    Title_maxlen = lists:max(lists:map(fun(A) -> length(A#test.title) end, T)),
    Io_str = "~w/~w: ~-" ++ integer_to_list(Title_maxlen) ++ "...s ",
    fun (Num, Title) ->
	    io:format(Io_str, [Num, Len, Title])
    end.


report_status(Status) ->
    io:format("~s~n", [Status]).


start() ->
    T = [#test{id="01",
	       title="Test TMPL_VAR",
	       data=[{"title", "There is a title here"}]},
	 #test{id="06",
	       title="Test TMPL_VAR with integer in data",
	       data=[{"var1", 17}]},
	 #test{id="07",
	       title="Test TMPL_VAR with float in data",
	       data=[{"var1", 17.0}]},
	 #test{id="02",
	       title="Test TMPL_VAR with no data",
	       data=[]},
	 #test{id="03",
	       title="Test TMPL_LOOP",
	       data=[{"loop1", [[{"v1", "1"}],
				[{"v1", "2"}]]}]},
	 #test{id="04",
	       title="Test TMPL_LOOP with an empty loop data",
	       data=[{"loop1", []}]},
	 #test{id="05",
	       title="Test TMPL_LOOP without needed var",
	       data=[{"loop1", [[{"v1", "1"}],
				[]]}]},
	 #test{id="08",
	       title="Test inner TMPL_LOOP",
	       data=[{"loop1",
		      [[{"v1", "1"},
			{"loop2",
			 [[{"v2", "asdf"}],
			  [{"v2", "qwer"}]]}],
		       [{"v1", "2"},
			{"loop2",
			 [[{"v2", ";lkj"}],
			  [{"v2", "poiu"}]]}
		       ]
		      ]}
		    ]},
	 #test{id="09",
	       title="Test TMPL_IF",
	       data=[{"v1", true},
		     {"v2", false}
		    ]},
	 #test{id="10",
	       title="Test TMPL_IF with no var data",
	       data=[]},
	 #test{id="11",
	       title="Test TMPL_IF with TMPL_ELSE",
	       data=[{"v1", true},
		     {"v2", false}
		    ]},
	 #test{id="12",
	       title="Test TMPL_IF with TMPL_ELSE with no var data",
	       data=[]}
	],
    Report_fun = make_report_fun(T),
    Passed_num = do_test(T, 1, 0, Report_fun),
    io:format("~nPassed: ~w/~w~n", [Passed_num, length(T)]).
