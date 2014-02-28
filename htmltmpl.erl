%%%
%%% Version 0.3.0
%%%
-module(htmltmpl).
-export([open/1, apply/2, apply/3]).

-record(phrase, {text, func}).

%%
%% The template state
%%  phrases - a list of lists
%%  priv - a list
%%
-record(tstate, {phrases=[], priv=[]}).

-record(tspriv, {tag, attrs=[], attr_name, attr_end_func, postend_func, data}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read a template from a file and prepare it.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Get a next template char.
%% Now only a reading from a file is implemented.
%% Get:
%%  TH - a template handle
%%
tmpl_get_char(TH) ->
    case TH of
	{fh, FH} ->
	    Char = io:get_chars(FH, "", 1),
	    {Char, TH}
    end.


%%
%% Find phrases from Phrases that match Str
%% Get:
%%  Phrases - a list of {PHRASE_TEXT, FUNC_NAME}
%% Return:
%%  {State,
%%   Ph_match} - State - 0, 1 or 2
%%               Ph_match - a list of matched phrases
%%
tmpl_match_phrase(Phrases, Str) ->
    tmpl_match_phrase(Phrases, Str, []).

tmpl_match_phrase([], _Str, Ph_match) ->
    case length(Ph_match) == 0 of
	true ->
	    {0, []};
	false ->
	    {1, Ph_match}
    end;
tmpl_match_phrase([H|T], Str, Ph_match) ->
    Ph_txt = H#phrase.text,
    Str_len = string:len(Str),
    case string:equal(Str, string:substr(Ph_txt, 1, Str_len)) of
	true ->
	    case Str_len == string:len(Ph_txt) of
		true ->
		    {2, H};
		false ->
		    tmpl_match_phrase(T, Str, [H|Ph_match])
	    end;
	false ->
	    tmpl_match_phrase(T, Str, Ph_match)
    end.


%%
%% Tag default handlers
%%
hdlr_tag(Tag, Tag_attr_names, Tag_attr_hdlr, Tag_postend_hdlr,
	 Tmpl, Tstate, Str_buf) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    Phrases_new = [mk_attr_phrases(Tag_attr_names)|Tstate#tstate.phrases],
    My_priv = #tspriv{tag=Tag,
		      attr_end_func=Tag_attr_hdlr,
		      postend_func=Tag_postend_hdlr},
    Priv_new = [My_priv|Tstate#tstate.priv],
    Tstate_new = #tstate{phrases=Phrases_new, priv=Priv_new},
    {continue, Tmpl_new, Tstate_new}.


hdlr_tag_attr(Tmpl, Tstate, _Str_buf, Str) ->
    [_|Ph] = Tstate#tstate.phrases,
    Priv = tsprivs_set_attr_name(Tstate#tstate.priv, Str),
    Phrases = [#phrase{text=">",
		       func=fun(T, TS, SB, S) ->
				    hdlr_tag_attr_end(T, TS, SB, S)
			    end}],
    Tstate_new = #tstate{phrases=[Phrases|Ph], priv=Priv},
    {continue, Tmpl, Tstate_new}.


hdlr_tag_attr_end(Tmpl, Tstate, Str_buf, Str) ->
    [My_priv|Priv] = Tstate#tstate.priv,
    My_priv_new = tspriv_set_attr(My_priv, Str_buf),
    Tstate_new = #tstate{phrases=Tstate#tstate.phrases, priv=[My_priv_new|Priv]},
    Func = My_priv_new#tspriv.attr_end_func,
    Func(Tmpl, Tstate_new, Str_buf, Str).


%%
%% Make a phrases list from an attributes list
%% Get:
%%  Attr_names - a list of attribute names
%% Return:
%%  [#phrase{...}, ...] - a list of phrase records
%%
mk_attr_phrases(Attr_names) ->
    mk_attr_phrases(Attr_names, []).

mk_attr_phrases([], Phrases) ->
    Phrases;
mk_attr_phrases([H|T], Phrases) ->
    mk_attr_phrases(T, [#phrase{text=H ++ "=",
				func=fun(T_, TS, SB, S) ->
					     hdlr_tag_attr(T_, TS, SB, S)
				     end}|Phrases]).


%%
%% Set a current attribute name in tstate current priv
%% Get:
%%  Priv_list - a priv list
%%  Name - a name of current attribute
%% Return:
%%  Priv_list_new - a priv list with changed name in a current priv
%%
tsprivs_set_attr_name([My_priv|Priv], Name) ->
    Attr_name = string:substr(Name, 1, string:len(Name) - 1),
    My_priv_new = My_priv#tspriv{attr_name=Attr_name},
    [My_priv_new|Priv].

tspriv_set_attr(My_priv, Value) ->
    Attrs = [{My_priv#tspriv.attr_name, Value}|My_priv#tspriv.attrs],
    My_priv#tspriv{attrs=Attrs, attr_name=""}.


%%
%% TMPL_VAR
%%
hdlr_var(Tmpl, Tstate, Str_buf, _Str) ->
    hdlr_tag(tag_var,
	     ["NAME"],
	     fun(T, TS, SB, S) ->
		     hdlr_var_attr_end(T, TS, SB, S)
	     end,
	     fun(T, TS) ->
		     {T, TS}
	     end,
	     Tmpl, Tstate, Str_buf).


hdlr_var_attr_end(Tmpl, Tstate, _Str_buf, _Str) ->
    [_|T] = Tstate#tstate.phrases,
    [My_priv|Priv] = Tstate#tstate.priv,
    Tstate_new = #tstate{phrases=T, priv=Priv},
    Tmpl_new = [{tag_var, My_priv#tspriv.attrs, []}|Tmpl],
    {continue, Tmpl_new, Tstate_new}.


%%
%% TMPL_LOOP
%%
hdlr_loop(Tmpl, Tstate, Str_buf, _Str) ->
    hdlr_tag(tag_loop,
	     ["NAME"],
	     fun(T, TS, SB, S) ->
		     hdlr_loop_attr_end(T, TS, SB, S)
	     end,
	     fun(T, TI, TS) ->
		     hdlr_loop_postend(T, TI, TS)
	     end,
	     Tmpl, Tstate, Str_buf).


hdlr_loop_attr_end(Tmpl, Tstate, _Str_buf, _Str) ->
    [_|T] = Tstate#tstate.phrases,
    Tstate_new = #tstate{phrases=T, priv=Tstate#tstate.priv},
    {start_inner, Tmpl, Tstate_new}.


hdlr_loop_end(Tmpl, Tstate, Str_buf, _Str) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    {stop, Tmpl_new, Tstate}.


hdlr_loop_postend(Tmpl, Tmpl_inner, Tstate) ->
    [Mypriv|Priv] = Tstate#tstate.priv,
    Tmpl_new = [{Mypriv#tspriv.tag,
		Mypriv#tspriv.attrs,
		lists:reverse(Tmpl_inner)}|Tmpl],
    Tstate_new = Tstate#tstate{priv=Priv},
    {Tmpl_new, Tstate_new}.


%%
%% TMPL_IF
%%
hdlr_if(Tmpl, Tstate, Str_buf, _Str) ->
    hdlr_tag(tag_if,
	     ["NAME"],
	     fun(T, TS, SB, S) ->
		     hdlr_if_attr_end(T, TS, SB, S)
	     end,
	     fun(T, TI, TS) ->
		     hdlr_if_postend(T, TI, TS)
	     end,
	     Tmpl, Tstate, Str_buf).


hdlr_if_attr_end(Tmpl, Tstate, _Str_buf, _Str) ->
    [_|T] = Tstate#tstate.phrases,
    Tstate_new = #tstate{phrases=T, priv=Tstate#tstate.priv},
    {start_inner, Tmpl, Tstate_new}.


hdlr_if_else(Tmpl, Tstate, Str_buf, _Str) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    [Mypriv|Priv] = Tstate#tstate.priv,
    Mypriv_new = Mypriv#tspriv{data=Tmpl_new},
    Tstate_new = Tstate#tstate{priv=[Mypriv_new|Priv]},
    {continue, [], Tstate_new}.


hdlr_if_end(Tmpl, Tstate, Str_buf, _Str) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    {stop, Tmpl_new, Tstate}.


hdlr_if_postend(Tmpl, Tmpl_inner, Tstate) ->
    [Mypriv|Priv] = Tstate#tstate.priv,
    case Mypriv#tspriv.data of
	undefined ->
	    Data = [{data, lists:reverse(Tmpl_inner)}];
	Tmpl_prev ->
	    Data = [{data, lists:reverse(Tmpl_prev)},
		    {data_else, lists:reverse(Tmpl_inner)}]
    end,
    Tmpl_new = [{Mypriv#tspriv.tag,
		Mypriv#tspriv.attrs,
		Data}|Tmpl],
    Tstate_new = Tstate#tstate{priv=Priv},
    {Tmpl_new, Tstate_new}.


%%
%% TMPL_UNLESS
%%
hdlr_unless(Tmpl, Tstate, Str_buf, _Str) ->
    hdlr_tag(tag_unless,
	     ["NAME"],
	     fun(T, TS, SB, S) ->
		     hdlr_if_attr_end(T, TS, SB, S)
	     end,
	     fun(T, TI, TS) ->
		     hdlr_if_postend(T, TI, TS)
	     end,
	     Tmpl, Tstate, Str_buf).


%%
%% Process a template state
%% Get:
%%  0/1/2 - a state
%%  Phrase - a list of matched phrases or a single phrase if one
%%  Tmpl - a template
%%  Tstate - a template state
%%  Str_buf - a current str buffer
%%  Str - a current matching str
%% Return:
%%  Act - an action
%%  Tmpl - a new template
%%  Tstate - a new template state
%%  Str_buf - a new str buffer
%%  Str - a new str
%%
proc_tmpl_state(0, _Phrase, Tmpl, Tstate, Str_buf, Str) ->
    {continue, Tmpl, Tstate, Str_buf ++ Str, ""};
proc_tmpl_state(1, _Phrase, Tmpl, Tstate, Str_buf, Str) ->
    {continue, Tmpl, Tstate, Str_buf, Str};
proc_tmpl_state(2, Phrase, Tmpl, Tstate, Str_buf, Str) ->
    Func = Phrase#phrase.func,
    {Act, Tmpl_new, Tstate_new} = Func(Tmpl, Tstate, Str_buf, Str),
    {Act, Tmpl_new, Tstate_new, "", ""}.


%%
%% Make a template
%% Get:
%%  TH - an input template handler
%%  Tmpl - a generated template
%%  Tstate - a template state
%%  Str_buf - a str buffer
%%  Str - a matching str
%% Return:
%%  Tmpl - a generated template
%%  Tstate - a new template state
%%
make_tmpl(TH, Tmpl, Tstate, Str_buf, Str) ->
    [H|_T] = Tstate#tstate.phrases,
    {State_new, Ph_match} = tmpl_match_phrase(H, Str),
    {Act,
     Tmpl_new,
     Tstate_new,
     Str_buf_new,
     Str_new} = proc_tmpl_state(State_new,
				Ph_match,
				Tmpl,
				Tstate,
				Str_buf,
				Str),
    case Act of
	start_inner ->
	    % Process a next data as an inner template
	    {Tmpl_new1, Tstate_new1} = make_tmpl(TH, [], Tstate_new,
						 "",
						 ""),
	    [Mypriv|_] = Tstate_new1#tstate.priv,
	    Func=Mypriv#tspriv.postend_func,
	    {Tmpl_new2, Tstate_new2} = Func(Tmpl_new, Tmpl_new1, Tstate_new1),
	    make_tmpl(TH, Tmpl_new2, Tstate_new2, "","");
	stop ->
	    % Stop of processing an inner template
	    {Tmpl_new, Tstate_new};
	continue ->
	    {Char, TH_new} = tmpl_get_char(TH),
	    case Char of
		eof ->
		    {[{text, Str_buf_new}|Tmpl], Tstate_new};
		_ ->
		    make_tmpl(TH_new, Tmpl_new, Tstate_new, Str_buf_new,
			      Str_new ++ Char)
	    end
    end.

make_tmpl(TH, Tmpl) ->
    Phrases = [#phrase{text="<TMPL_VAR ",
		       func=fun(T, TS, SB, S) -> hdlr_var(T, TS, SB, S) end},
	       #phrase{text="<TMPL_LOOP ",
		       func=fun(T, TS, SB, S) -> hdlr_loop(T, TS, SB, S) end},
	       #phrase{text="</TMPL_LOOP>",
		       func=fun(T, TS, SB, S) -> hdlr_loop_end(T, TS, SB, S) end},
	       #phrase{text="<TMPL_IF ",
		       func=fun(P, T, SB, S) -> hdlr_if(P, T, SB, S) end},
	       #phrase{text="<TMPL_ELSE>",
		       func=fun(P, T, SB, S) -> hdlr_if_else(P, T, SB, S) end},
	       #phrase{text="</TMPL_IF>",
		       func=fun(P, T, SB, S) -> hdlr_if_end(P, T, SB, S) end},
	       #phrase{text="<TMPL_UNLESS ",
		       func=fun(P, T, SB, S) -> hdlr_unless(P, T, SB, S) end},
	       #phrase{text="</TMPL_UNLESS>",
		       func=fun(P, T, SB, S) -> hdlr_if_end(P, T, SB, S) end}],
    Tstate = #tstate{phrases=[Phrases], priv=[]},
    {Tmpl_new, _Tstate} = make_tmpl(TH, Tmpl, Tstate, "", ""),
    Tmpl_new.

make_tmpl(TH) ->
    Tmpl = make_tmpl(TH, []),
    lists:reverse(Tmpl).

open(Fname) ->
    case file:open(Fname, [read]) of
	{ok, FH} ->
	    Tmpl = make_tmpl({fh, FH}),
	    case file:close(FH) of
		ok ->
		    {ok, Tmpl};
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Apply a data to a compiled template
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Get a tag attribute value
%% Get:
%%  Tag - a tag
%%  Attr_name - an attribute name
%% Return:
%%  Attr_val - an attribute value
%%
tag_attr_get(Tag, Attr_name) ->
    {_Name, Attrs, _Data} = Tag,
    case lists:keyfind(Attr_name, 1, Attrs) of
	false ->
	    {error, "Attribute " ++ Attr_name ++ " not found"};
	{Attr_name, Value} ->
	    {ok, Value}
    end.


%%
%% Var
%%
subst_tmpl_var(Var, Data) ->
    {ok, Var_name} = tag_attr_get(Var, "NAME"),
    case lists:keyfind(Var_name, 1, Data) of
	false ->
	    {ok, ""};
	{Var_name, Value} when is_list(Value) ->
	    {ok, Value};
	{Var_name, Value} when is_float(Value) ->
	    {ok, float_to_list(Value)};
	{Var_name, Value} when is_integer(Value) ->
	    {ok, integer_to_list(Value)}
    end.


%%
%% Loop
%%
subst_tmpl_loop(Loop, Data) ->
    {ok, Loop_name} = tag_attr_get(Loop, "NAME"),
    {_, _, Loop_tmpl} = Loop,
    case lists:keyfind(Loop_name, 1, Data) of
	false ->
	    {ok, ""};
	{Loop_name, Value} ->
	    subst_tmpl_loop_(Loop_tmpl, Value)
    end.


subst_tmpl_loop_(Loop, Data) ->
    subst_tmpl_loop_(Loop, Data, "").

subst_tmpl_loop_(_, [], Str) ->
    {ok, Str};
subst_tmpl_loop_(Loop, [H|T], Str) ->
    Output = htmltmpl:apply(Loop, H, ""),
    subst_tmpl_loop_(Loop, T, Str ++ Output).


%%
%% If
%%
subst_tmpl_if(If, Data) ->
    {ok, If_name} = tag_attr_get(If, "NAME"),
    case lists:keyfind(If_name, 1, Data) of
	false ->
	    subst_tmpl_if_sect(If, data_else, Data);
	{If_name, Value} ->
	    case Value of
		true ->
		    subst_tmpl_if_sect(If, data, Data);
		false ->
		    subst_tmpl_if_sect(If, data_else, Data)
	    end
    end.


subst_tmpl_if_sect(If, Sect, Data) ->
    {_, _, If_data} = If,
    case lists:keyfind(Sect, 1, If_data) of
	false ->
	    {ok, ""};
	{Sect, Tmpl} ->
	    {ok, htmltmpl:apply(Tmpl, Data, "")}
    end.


%%
%% Unless
%%
subst_tmpl_unless(Unless, Data) ->
    {ok, Unless_name} = tag_attr_get(Unless, "NAME"),
    case lists:keyfind(Unless_name, 1, Data) of
	false ->
	    subst_tmpl_if_sect(Unless, data, Data);
	{Unless_name, Value} ->
	    case Value of
		true ->
		    subst_tmpl_if_sect(Unless, data_else, Data);
		false ->
		    subst_tmpl_if_sect(Unless, data, Data)
	    end
    end.


%%
%% Apply a data to a compiled template
%% Get:
%%  Tmpl - a compiled template
%%  Data - a data to be inserted into a template
%% Return:
%%  Tmpl_str - a final result
%%
apply(Tmpl, Data) ->
    htmltmpl:apply(Tmpl, Data, "").

apply([], _, Str) ->
    Str;
apply([H|Tmpl], Data, Str) ->
    case H of
	{text, S} ->
	    htmltmpl:apply(Tmpl, Data, Str ++ S);
	{tag_var, _, _} ->
	    {ok, Var_val} = subst_tmpl_var(H, Data),
	    htmltmpl:apply(Tmpl, Data, Str ++ Var_val);
	{tag_loop, _, _} ->
	    {ok, Loop_val} = subst_tmpl_loop(H, Data),
	    htmltmpl:apply(Tmpl, Data, Str ++ Loop_val);
	{tag_if, _, _} ->
	    {ok, If_val} = subst_tmpl_if(H, Data),
	    htmltmpl:apply(Tmpl, Data, Str ++ If_val);
	{tag_unless, _, _} ->
	    {ok, Unless_val} = subst_tmpl_unless(H, Data),
	    htmltmpl:apply(Tmpl, Data, Str ++ Unless_val)
    end.

