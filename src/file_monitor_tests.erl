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
%% $Id: file_monitor.erl 283 2008-12-06 12:20:44Z rcarlsson $ 
%%
%% @private (for now)
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @doc Unit tests for the Erlang file monitoring service

-module(file_monitor_tests).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(SERVER, file_monitor).
-define(MSGTAG, ?SERVER).

%% Basic tests: these start and stop the server for each test
basic_test_() ->
    case os:type() of
	{unix,_} ->
	    {foreach,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     [{with, [fun return_value_test/1]},
	      {with, [fun flatten_path_test/1]},
	      {with, [fun no_file_test/1]},
	      {with, [fun no_dir_test/1]},
	      {with, [fun existing_dir_test/1]},
	      {with, [fun existing_file_test/1]},
	      {with, [fun notdir_test/1]},
	      {with, [fun dir_as_file_test/1]}
	     ]
	    };
	_ ->
	    []
    end.

%% All the below tests run only on unix-like platforms

return_value_test(Server) ->
    Path = "/tmp/nonexisting",  % flat string
    MonitorResult = ?SERVER:monitor_file(Server, Path, self()),
    ?assertMatch({ok, Path, Ref} when is_reference(Ref), MonitorResult),
    {ok, _, MonitorRef} = MonitorResult,
    ?assertMatch(ok, ?SERVER:demonitor(Server, MonitorRef)),
    ?assertMatch({ok, Path, Ref} when is_reference(Ref),
		 ?SERVER:monitor_dir(Server, Path, self())).

flatten_path_test(Server) ->
    Path = ["/","tmp","/","foo"],
    ?assertMatch({ok, "/tmp/foo", _},
		 ?SERVER:monitor_file(Server, Path, self())),
    ?assertMatch({ok, "/tmp/foo", _},
		 ?SERVER:monitor_dir(Server, Path, self())).

no_file_test(Server) ->
    Path = "/tmp/nonexisting",
    {ok, Path, Ref} = ?SERVER:monitor_file(Server, Path, self()),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, file, enoent}},
			 Msg)
    end.

no_dir_test(Server) ->
    Path = "/tmp/nonexisting",
    {ok, Path, Ref} = ?SERVER:monitor_dir(Server, Path, self()),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref, {error, Path, directory, enoent}},
			 Msg)
    end.

existing_dir_test(Server) ->
    Path = "/etc",
    {ok, Path, Ref} = ?SERVER:monitor_dir(Server, Path, self()),
    receive
	Msg ->
	    %% we should get a nonempty list of directory entries
	    ?assertMatch({?MSGTAG, Ref,
			  {exists, Path, directory, #file_info{}, Es}}
			 when (is_list(Es) and (Es =/= [])), Msg)
    end.

existing_file_test(Server) ->
    Path = "/etc/passwd",
    {ok, Path, Ref} = ?SERVER:monitor_file(Server, Path, self()),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {exists, Path, file, #file_info{}, []}}, Msg)
    end.

notdir_test(Server) ->
    Path = "/etc/passwd",
    {ok, Path, Ref} = ?SERVER:monitor_dir(Server, Path, self()),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, directory, enotdir}}, Msg)
    end.

dir_as_file_test(Server) ->
    Path = "/etc",
    {ok, Path, Ref} = ?SERVER:monitor_file(Server, Path, self()),
    receive
	Msg ->
	    %% we should get an empty list of directory entries,
	    %% since we are just monitoring it as a file
	    ?assertMatch({?MSGTAG, Ref,
			  {exists, Path, file, #file_info{}, []}}, Msg)
    end.


%% Event tests: this runs the same server over a group of tests
event_test_() ->
    case os:type() of
	{unix,_} ->
	    {setup,
	     fun new_test_server/0,
	     fun stop_test_server/1,
	     fun (Server) ->
		     {setup, local,
		      fun () ->
			      Path = "/tmp/filemonitortestfile",
			      remove_file(Path),
			      {ok, Path, Ref} =
				  ?SERVER:monitor_file(Server, Path,
						       self()),
			      receive
				  Msg ->
				      ?assertMatch({?MSGTAG, Ref,
						    {error, Path,
						     file, enoent}},
						   Msg)
			      end,
			      {Path, Ref}
		      end,
		      {with,
		       [fun create_file_subtest/1,
			fun delete_file_subtest/1,
			fun create_file_subtest/1,
			fun delete_file_subtest/1,
			fun create_file_subtest/1,
 			fun touch_file_subtest/1,
 			fun touch_file_subtest/1,
 			fun touch_file_subtest/1,
			fun delete_file_subtest/1
		       ]}
		     }
	     end
	    };
	_ ->
	    []
    end.

create_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    write_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {changed, Path, file,
			   #file_info{type=regular}, []}}, Msg)
    end,
    assert_empty_mailbox().    

delete_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    remove_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
			  {error, Path, file, enoent}}, Msg)
    end,
    assert_empty_mailbox().    

touch_file_subtest({Path, Ref}) ->
    assert_empty_mailbox(),
    touch_file(Path),
    receive
	Msg ->
	    ?assertMatch({?MSGTAG, Ref,
 			  {changed, Path, file,
			   #file_info{type=regular}, []}}, Msg)
    end,
    assert_empty_mailbox().    

%% test utilities

new_test_server() ->
    {ok, Server} = ?SERVER:start(undefined, [{poll_time, 100}]),
    Server.

stop_test_server(Server) ->
    ?SERVER:stop(Server).

write_file(Path) ->
    case file:write_file(Path, <<"this is a test\n">>) of
	ok -> ok;
	{error, Err} -> throw({could_not_write, Err, Path})
    end.    

touch_file(Path) ->
    %% we must ensure that the new timestamp is at least one second
    %% older than any previous write, otherwise the change may not be
    %% detected due to the low timestamp resolution
    receive after 1100 -> ok end,
    write_file(Path).

remove_file(Path) ->
    case file:delete(Path) of
	ok -> ok;
	{error, enoent} -> ok;
	{error, Err} -> throw({could_not_delete, Err, Path})
    end.

assert_empty_mailbox() ->
    receive MsgX -> throw({unexpected_message, MsgX})
    after 0 -> ok
    end.
