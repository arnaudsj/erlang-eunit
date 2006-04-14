%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Richard Carlsson.''
%%
%% File: eunit_server.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc EUnit server process

-module(eunit_server).

-export([start/1, stop/1, start_test/4]).

-include("eunit.hrl").
-include("eunit_internal.hrl").


start(Server) when is_atom(Server) ->
    ensure_started(Server).

stop(Server) ->
    command(Server, stop).

start_test(Server, Super, T, Options) ->
    command(Server, {test, Super, T, Options}).


%% This makes sure the server is started before sending the command, and
%% returns {ok, Result} if the server accepted the command or {error,
%% server_down} if the server process crashes. If the server does not
%% reply, this function will wait until the server is killed.

command(Server, Cmd) ->
    if is_atom(Server), Cmd /= stop -> ensure_started(Server);
       true -> ok
    end,
    if is_pid(Server) -> command_1(Server, Cmd);
       true ->
	    case whereis(Server) of
		undefined -> {error, server_down};
		Pid -> command_1(Pid, Cmd)
	    end
    end.

command_1(Pid, Cmd) when is_pid(Pid) ->
    Pid ! {command, self(), Cmd},
    command_wait(Pid, 1000, undefined).

command_wait(Pid, Timeout, Monitor) ->
    receive
	{Pid, Result} -> Result;
	{'DOWN', Monitor, process, Pid, _R} -> {error, server_down}
    after Timeout ->
	    %% avoid creating a monitor unless some time has passed
	    command_wait(Pid, infinity, erlang:monitor(process, Pid))
    end.

%% Starting the server

ensure_started(Name) ->
    ensure_started(Name, 5).

ensure_started(Name, N) when N > 0 ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_start(Name, Parent) end),
	    receive
		{Pid, ok} ->
		    Pid;
		{Pid, error} ->
		    receive after 200 -> ensure_started(N - 1) end
	    end;
	Pid ->
	    Pid
    end;
ensure_started(_, _) ->
    throw(no_server).

server_start(Name, Parent) ->
    Pid = self(),
    try register(Name, Pid) of
	true ->
	    Parent ! {Pid, ok},
	    server_init(Name)
    catch
	_:_ ->
	    Parent ! {Pid, error},
	    exit(error)
    end.

-record(state, {name, stopped}).

server_init(Name) ->
    server_loop(dict:new(), #state{stopped = false, name = Name}).

server_loop(Jobs, St) ->
    server_check_exit(Jobs, St),
    receive
	{done, Reference, _Pid} ->
	    server_loop(handle_done(Reference, Jobs), St);
	{command, From, _Cmd} when St#state.stopped ->
	    From ! {self(), stopped};
	{command, From, Cmd} ->
	    server_command(From, Cmd, Jobs, St)
    end.    

server_check_exit(Jobs, St) ->
    case dict:size(Jobs) of
	0 when St#state.stopped -> exit(normal);
	_ -> ok
    end.

server_command(From, {test, Super, T, Options}, Jobs, St) ->
    Reference = start_test(T, Options, Super),
    server_command_reply(From, {ok, Reference}),
    server_loop(dict:store(Reference, From, Jobs), St);
server_command(From, stop, Jobs, St) ->
    %% unregister the server name and let remaining jobs finish
    server_command_reply(From, {error, stopped}),
    catch unregister(St#state.name),
    server_loop(Jobs, St#state{stopped = true});
server_command(From, Cmd, Jobs, St) ->
    server_command_reply(From, {error, {unknown_command, Cmd}}),
    server_loop(Jobs, St).

server_command_reply(From, Result) ->
    From ! {self(), Result}.

start_test(T, Options, Super) ->
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_value(order, Options, inorder),
    {Reference, _Pid} = eunit_proc:start(T, Order, Super),
    Reference.

handle_done(Reference, Jobs) ->
    case dict:find(Reference, Jobs) of
	{ok, Pid} ->
	    Pid ! {done, Reference},
	    dict:erase(Reference, Jobs);
	error ->
	    Jobs
    end.
