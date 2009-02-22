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

-export([start/1, start/2]).


-record(state, {verbose = false,
		succeed = 0,
		fail = 0,
		abort = 0,
		skip = false,
		indent = 0,
		prefix       % prefix of cancelled tests
	       }).

start(List) ->
    start(List, []).

start(List, Options) ->
    St = #state{verbose = proplists:get_bool(verbose, Options)},
    Id = [],
    spawn(fun () -> init(Id, List, St) end).

init(Id, List, St0) ->
    receive
	{start, Reference} ->
	    if St0#state.verbose -> print_header();
	       true -> ok
	    end,
	    St = entry_begin(Id, "", List, reset_prefix(St0)),
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

reset_prefix(St) ->
    St#state{prefix = [-1]}.  % never matches real test id:s

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

wait(Id, Type, St) ->
    %%?debugVal({waiting_for, Id}),
    case lists:prefix(St#state.prefix, Id) of
	true ->
	    %% cancelled due to parent
	    %%?debugVal({cancelled_by_prefix, Id}),
	    {{cancel, undefined}, St};
	false ->
	    receive
		{status, Id, {cancel, Reason}} ->
		    %%?debugVal({got_cancel, Id, Reason}),
		    {{cancel, Reason}, St#state{prefix=Id}};
		{status, Id1, {cancel, _Reason}} ->
		    %%?debugVal({got_cancel, Id1, Reason}),
		    %% Id1 should be parent of Id in this case
		    {{cancel, undefined}, St#state{prefix=Id1}};
		{status, Id, {progress, Type, Data}} ->
		    %%?debugVal({got_status, Id, Data}),
		    {{progress, Data}, reset_prefix(St)}
	    end
    end.

entry({item, Id, Desc, Loc}, St) ->
    entry_begin(Id, Desc, Loc, St);
entry({group, Id, Desc, Es}, St) ->
    entry_begin(Id, Desc, Es, St).

tests([E | Es], St) ->
    tests(Es, entry(E, St));
tests([], St) ->
    St.

entry_begin(Id, Desc, Data, St) ->
    case wait(Id, 'begin', St) of
	{{progress, test}, St1} ->
	    TestBegin = fun () ->
				print_test_begin(St1#state.indent,
						 Data, Desc)
			end,
	    if St#state.verbose -> TestBegin();
	       true -> ok
	    end,
	    test_end(Id, TestBegin, St1);
	{{progress, group}, St1} ->
	    I = St1#state.indent,
	    St2 = if Desc /= "", St1#state.verbose ->
			  print_group_start(I, Desc),
			  St1#state{indent = I + 1};
		     true ->
			  St1
		  end,
	    group_end(Id, I, Desc, tests(Data, St2));
	{{cancel, Reason}, St1} ->
	    TestBegin = fun () ->
				print_test_begin(St1#state.indent,
						 Data, Desc)
			end,
	    if St1#state.verbose -> TestBegin();
	       true -> ok
	    end,
	    if St1#state.verbose -> print_test_cancel(Reason);
	       Reason /= undefined ->
		    TestBegin(),
		    print_test_cancel(Reason);
	       true -> ok
	    end,
	    St1#state{skip = true};
	{{cancell, Reason}, St1} ->
	    I = St1#state.indent,
	    if Desc /= "", St1#state.verbose ->
		    print_group_cancel(I, Reason);
	       Desc /= "" ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason);
	       true ->
		    ok
	    end,
	    St1#state{indent = I, skip = true}
    end.

%% group_begin(Id, Desc, Es, St0) ->
%%     case wait(Id, 'begin', St0) of
%%     end.

test_end(Id, Begin, St) ->
    case wait(Id, 'end', St) of
	{{progress, {Result, Time, Output}}, St1} ->
	    if Result =:= ok ->
		    if St#state.verbose -> print_test_end(Time);
		       true -> ok
		    end,
		    St1#state{succeed = St1#state.succeed + 1};
	       true ->
		    if St#state.verbose -> ok;
		       true -> Begin()
		    end,
		    print_test_error(Result, Output),
		    St1#state{fail = St1#state.fail + 1}
	    end;
	{{cancel, Reason}, St1} ->
	    if St#state.verbose -> ok;
	       true -> Begin()
	    end,
	    print_test_cancel(Reason),
	    St1#state{abort = St1#state.abort + 1}
    end.

group_end(Id, I, Desc, St) ->
    case wait(Id, 'end', St) of
	{{progress, {_Count, Time, _Output}}, St1} ->
	    if Desc /= "", St#state.verbose ->
		    print_group_end(St1#state.indent, Time);
	       true ->
		    ok
	    end,
	    St1#state{indent = I};
	{{cancel, undefined}, St1} ->
	    %% "skipped" message is not interesting here
	    St1#state{indent = I};
	{{cancel, Reason}, St1} ->
	    if Desc /= "", St1#state.verbose ->
		    print_group_cancel(I, Reason);
	       true ->
		    print_group_start(I, Desc),
		    print_group_cancel(I, Reason)
	    end,
	    St1#state{indent = I}
    end.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
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
    D = if Desc =:= "" -> "";
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

