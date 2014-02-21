-module(ex).
-export([start/0]).


start() ->
    {ok, Tmpl} = htmltmpl:open("ex.html"),
    Output = htmltmpl:apply(Tmpl, [{"text", "f"}, {"text_here", "123"},
				   {"title", "qweradf"},
				   {"show_this", true},
				   {"l1", [[{"inner_var", "1"}],
					   [{"inner_var", "2"}]]}]),
    io:format("~s~n", [Output]).
