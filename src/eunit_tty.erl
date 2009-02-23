%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

-module(eunit_tty).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/0, start/1]).


-record(state, {verbose = false,
		succeed = 0,
		fail = 0,
		abort = 0,
		skip = false,
		indent = 0,
		prefix       % prefix of cancelled tests
	       }).

start() ->
    start([]).

start(Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options)},
    Id = [],
    spawn(fun () -> init(Id, St) end).

init(Id, St0) ->
    receive
	{start, Reference} ->
	    if St0#state.verbose -> print_header();
	       true -> ok
	    end,
	    St = expect(Id, undefined, St0),
	    receive
		{stop, Reference, ReplyTo} ->
		    Result = if St#state.fail =:= 0,
				St#state.abort =:= 0,
				St#state.skip =:= false ->
				     ok;
				true ->
				     error
			     end,
		    report(Result, St),
		    ReplyTo ! {result, Reference, Result},
		    ok
	    end
    end.

report(ok, St) ->
    if St#state.succeed =:= 0 ->
	    io:fwrite("  There were no tests to run.\n");
       true ->
	    if St#state.verbose -> print_bar();
	       true -> ok
	    end,
	    if St#state.succeed =:= 1 ->
		    io:fwrite("  Test successful.\n");
	       true ->
		    io:fwrite("  All ~w tests successful.\n",
			      [St#state.succeed])
	    end
    end;
report(error, St) ->
    print_bar(),
    io:fwrite("  Failed: ~w.  Aborted: ~w.  Succeeded: ~w.\n",
	      [St#state.fail, St#state.abort, St#state.succeed]),
    if St#state.skip =:= true ->
	    io:fwrite("One or more tests were skipped.\n");
       true -> ok
    end.

print_header() ->
    io:fwrite("======================== EUnit ========================\n").

print_bar() ->
    io:fwrite("=======================================================\n").


%% waiting for [..., M, N] begin
%% get:
%%      [..., M, N] begin test  -> expect [..., M, N] end    (test begin)
%%      [..., M, N] begin group -> expect [..., M, N, 1] end (group begin)
%%      [..., M] end -> expect [..., M+1] begin        (parent end)
%%      cancel([..., M])                               (parent cancel)
%%
%% waiting for [..., M, N] end
%% get:
%%      [..., M, N] end -> expect [..., M, N+1] begin    (seen end)
%%      cancel([..., M, N])                              (cancelled)

wait_for(Id, Type, ParentId) ->
    %%?debugVal({waiting_for, Id, Type}),
    receive
	{status, Id, {progress, Type, Data}} ->
	    %%?debugVal({got_status, Id, Data}),
	    {ok, Data};
	{status, ParentId, {progress, 'end', Data}} when Type =:= 'begin' ->
	    %%?debugVal({got_parent_end, ParentId, Data}),
	    {done, Data};
	{status, Id, {cancel, Reason}} when Type =:= 'end' ->
	    %%?debugVal({got_cancel, Id, Reason}),
	    {cancel, Reason};
	{status, ParentId, {cancel, _Reason}} ->
	    %%?debugVal({got_parent_cancel, ParentId, _Reason}),
	    {done, {cancel, _Reason}}
    end.

expect(Id, ParentId, St) ->
    case wait_for(Id, 'begin', ParentId) of
	{done, Data} ->
	    {done, Data, St};
	{ok, Msg} ->
	    case Msg of
		{group, Data} ->
		    group(Id, Data, St);
		{test, Data} ->
		    St1 = handle_begin(test, Id, Data, St),
		    case wait_for(Id, 'end', ParentId) of
			{cancel, Reason} ->
			    handle_cancel(test, Id, Data, Reason, St1);
			{ok, Result} ->
			    handle_end(test, Id, Data, Result, St1)
		    end
	    end
    end.

%% collect group items in order until group is done
group(Id, Data, St) ->
    St1 = handle_begin(group, Id, Data, St),
    group_loop(0, Id, Data, St1).

group_loop(N, Id, Data, St) ->
    N1 = N + 1,
    case expect(Id ++ [N1], Id, St) of
	{done, {cancel, Reason}, St1} ->
	    handle_cancel(group, Id, Data, Reason, St1);
	{done, Result, St1} ->
	    handle_end(group, Id, Data, Result, St1);
	St1 ->
	    group_loop(N1, Id, Data, St1)
    end.


handle_begin(group, _Id, {Desc, _Extra}=_Data, St) ->
    %%?debugVal({handle_group_begin, Id, _Data}),
    I = St#state.indent,
    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
	    print_group_start(I, Desc),
	    St#state{indent = I + 1};
       true ->
	    St
    end;
handle_begin(test, _Id, {Desc, Loc, Line}=_Data, St) ->
    %%?debugVal({handle_test_begin, Id, _Data}),
    if St#state.verbose ->
	    print_test_begin(St#state.indent, {Loc, Line}, Desc);
       true -> ok
    end,
    St.

handle_end(group, _Id, {Desc, _Extra}=_Data, {_Count, Time, _Output}=_Res, St) ->
    %%?debugVal({handle_group_end, Id, _Data, _Res}),
    I = St#state.indent,
    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
	    print_group_end(I, Time),
	    St#state{indent = I - 1};
       true ->
	    St
    end;
handle_end(test, _Id, Data, {Result, Time, Output}, St) ->
    %%?debugVal({handle_test_end, Id, Data, Result}),
    if Result =:= ok ->
	    if St#state.verbose -> print_test_end(Time);
	       true -> ok
	    end,
	    St#state{succeed = St#state.succeed + 1};
       true ->
	    if St#state.verbose ->
		    ok;
	       true ->
		    {Desc, Loc, Line}=Data,
		    print_test_begin(St#state.indent, {Loc, Line}, Desc)
	    end,
	    print_test_error(Result, Output),
	    St#state{fail = St#state.fail + 1}
    end.
    
handle_cancel(group, _Id, {Desc, _Extra}=_Data, Reason, St) ->
    %%?debugVal({handle_group_cancel, Id, _Data, Reason}),
    I = St#state.indent,
    if Reason =:= undefined ->
	    %% "skipped" message is not interesting here
	    St#state{indent = I - 1};
       true ->
	    if Desc =/= "", Desc =/= undefined, St#state.verbose ->
		    print_group_cancel(I, Reason);
	       true ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason)
	    end,
	    St#state{indent = I - 1}
    end;
handle_cancel(test, _Id, Data, Reason, St) ->
    %%?debugVal({handle_test_cancel, Id, Data, Reason}),
    if St#state.verbose -> ok;
       true ->
	    {Desc, Loc, Line}=Data,
	    print_test_begin(St#state.indent, {Loc, Line}, Desc)
    end,
    print_test_cancel(Reason),
    St#state{abort = St#state.abort + 1}.


indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_N) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    io:fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    io:fwrite("[done in ~.3f s]\n", [Time/1000]);
       true ->
	    ok
    end.

print_test_begin(I, {{Module, Name, _Arity}, Line}, Desc) ->
    indent(I),
    L = if Line =:= 0 -> "";
	   true -> io_lib:fwrite("~w:", [Line])
	end,
    D = if Desc =:= "" ; Desc =:= undefined -> "";
	   true -> io_lib:fwrite(" (~s)", [Desc])
	end,
    io:fwrite("~s:~s ~s~s...", [Module, L, Name, D]).

print_test_end(Time) ->
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    io:fwrite("~sok\n", [T]).

print_test_error({error, Exception}, Output) ->
    io:fwrite("*failed*\n::~s",
	      [eunit_lib:format_exception(Exception)]),
    case Output of
	<<>> ->
	    io:put_chars("\n\n");
	<<Text:800/binary, _:1/binary, _/binary>> ->
	    io:fwrite("  output:<<\"~s\">>...\n\n", [Text]);
	_ ->
	    io:fwrite("  output:<<\"~s\">>\n\n", [Output])
    end;
print_test_error({skipped, Reason}, _) ->
    io:fwrite("*did not run*\n::~s\n",
	      [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:format("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:format("no such function: ~w:~w/~w", [M,F,A]).    

print_test_cancel(Reason) ->
    io:fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    io:fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    eunit_lib:format_error(Reason).
