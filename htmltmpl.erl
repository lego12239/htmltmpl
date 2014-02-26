%%%
%%% Version 0.1.0
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

-record(tspriv, {tag, attrs=[], attr_name, attr_end_func}).

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
hdlr_tag(Tag, Tag_attr_names, Tag_attr_hdlr, Tmpl, Tstate, Str_buf) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    Phrases_new = [mk_attr_phrases(Tag_attr_names)|Tstate#tstate.phrases],
    My_priv = #tspriv{tag=Tag,
		      attr_end_func=Tag_attr_hdlr},
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


%%
%% TMPL_IF
%%
hdlr_if(Tmpl, Tstate, Str_buf, _Str) ->
    hdlr_tag(tag_if,
	     ["NAME"],
	     fun(T, TS, SB, S) ->
		     hdlr_if_attr_end(T, TS, SB, S)
	     end,
	     Tmpl, Tstate, Str_buf).


hdlr_if_attr_end(Tmpl, Tstate, _Str_buf, _Str) ->
    [_|T] = Tstate#tstate.phrases,
    Tstate_new = #tstate{phrases=T, priv=Tstate#tstate.priv},
    {start_inner, Tmpl, Tstate_new}.


hdlr_if_end(Tmpl, Tstate, Str_buf, _Str) ->
    case string:len(Str_buf) of
	0 ->
	    Tmpl_new = Tmpl;
	_ ->
	    Tmpl_new = [{text, Str_buf}|Tmpl]
    end,
    {stop, Tmpl_new, Tstate}.


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
	    [My_priv|Priv] = Tstate_new1#tstate.priv,
	    Tmpl_new2 = [{My_priv#tspriv.tag,
			  My_priv#tspriv.attrs,
			  lists:reverse(Tmpl_new1)}|Tmpl_new],
	    Tstate_new2 = Tstate_new1#tstate{priv=Priv},
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
	       #phrase{text="</TMPL_IF>",
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
    tag_attr_get_(Attrs, Attr_name).

tag_attr_get_([], Attr_name) ->
    {error, "Attribute " ++ Attr_name ++ " not found"};
tag_attr_get_([H|T], Attr_name) ->
    {Name, Value} = H,
    case string:equal(Attr_name, Name) of
	true ->
	    {ok, Value};
	false ->
	    tag_attr_get_(T, Attr_name)
    end.


%%
%% Var
%%
subst_tmpl_var(Var, Data) ->
    {ok, Var_name} = tag_attr_get(Var, "NAME"),
    subst_tmpl_var_(Var_name, Data).

subst_tmpl_var_(_Var_name, []) ->
    {ok, ""};
subst_tmpl_var_(Var_name, [H|T]) ->
    {Name, Value} = H,
    case string:equal(Name, Var_name) of
	true when is_list(Value) ->
	    {ok, Value};
	true when is_float(Value) ->
	    {ok, float_to_list(Value)};
	true when is_integer(Value) ->
	    {ok, integer_to_list(Value)};
	false ->
	    subst_tmpl_var_(Var_name, T)
    end.


%%
%% Loop
%%
subst_tmpl_loop(Loop, Data) ->
    {ok, Loop_name} = tag_attr_get(Loop, "NAME"),
    {_, _, Loop_data} = Loop,
    subst_tmpl_loop(Loop_name, Loop_data, Data).

subst_tmpl_loop(_Loop_name, _, []) ->
    {ok, ""};
subst_tmpl_loop(Loop_name, Loop, [H|T]) ->
    {Name, Value} = H,
    case string:equal(Name, Loop_name) of
	true ->
	    subst_tmpl_loop_(Loop, Value, "");
	false ->
	    subst_tmpl_loop(Loop_name, Loop, T)
    end.

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
    {_, _, If_data} = If,
    subst_tmpl_if(If_name, If_data, Data, Data).

subst_tmpl_if(_If_name, _If, [], _Data_full) ->
    {ok, ""};
subst_tmpl_if(If_name, If, [H|T], Data_full) ->
    {Name, Value} = H,
    case string:equal(Name, If_name) of
	true ->
	    case Value of
		true ->
		    {ok, htmltmpl:apply(If, Data_full, "")};
		false ->
		    {ok, ""}
	    end;
	false ->
	    subst_tmpl_if(If_name, If, T, Data_full)
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
	    htmltmpl:apply(Tmpl, Data, Str ++ If_val)
    end.

