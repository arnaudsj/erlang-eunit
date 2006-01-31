%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Initial Developer of the Original Code is Micka�l R�mond.''
%%
%%     $Id$
%%
%% @copyright 2004-2005 Micka�l R�mond
%% @author Micka�l R�mond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@csd.uu.se>
%%   [http://www.csd.uu.se/~richardc/]
%% @private
%% @see eunit
%% @doc This module contains legacy code from EUnit version 1.1.

-module(eunit_old).

-export([run/2, erlfilename/1]).

%% Previously in eunit_lib.erl:
-export([log/4, error/4]).

-include("eunit_old.hrl").


%% Run all functions in the given list of modules that match the given pattern
run(Modules, Pattern) ->
    Result = run(Modules, Pattern, {0,0,[],[]}),
    print_results(Result),
    Result.

run([], _Pattern, State) ->
    ?log("Test finished~n", []),
    State;
run([Module|Modules], Pattern, State) ->
    ?log("Running unit tests in module [~p]", [Module]),
    Exports = Module:module_info(exports),
    Result = lists:foldr(fun(Export, CurrentState = {Passed, Failed, PassedList, FailedList}) ->
				 case run_module_tests(Module, Export, Pattern) of
				     ignore -> CurrentState;
				     passed -> {Passed+1, Failed, [{Module,Export}|PassedList], FailedList};
				     failed -> {Passed, Failed+1, PassedList, [{Module,Export}|FailedList]}
				 end
			 end,
			 State,
			 Exports),
    run(Modules, Pattern, Result).

%% Only run matching functions with 0 arity
%% Returns ignore, passed, failed
%% If pattern is all, run 
run_module_tests(Module, {Function,0}, all) ->
    run_test(Module, Function);
%% otherwise select function based on pattern {suffix, String} or {prefix, String}
run_module_tests(Module, {Function,0}, _Pattern = {Pos, StringPattern}) ->
    StringExport  = atom_to_list(Function),
    Suffix = length(StringExport) - length(StringPattern) + 1,
    case {Pos, string:rstr(StringExport, StringPattern)} of
	{_,0}            -> ignore; %% Function name does not match
	{suffix, Suffix} -> run_test(Module, Function);
	{prefix, 1}      -> run_test(Module, Function)
    end;
run_module_tests(_Module, _Export, _Pattern) ->
    ignore.

%% Return passed, failed
run_test(Module, Function) ->
    TestDesc = "Running test " ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Function) ++ "/0",
    case catch (Module:Function()) of
	{'EXIT', _} ->
	    ?log(TestDesc ++ " - failed.", []),
	    failed;
	_Other ->
	    ?log(TestDesc ++ " - passed.", []),
	    passed
    end.

print_results({Passed, Failed, _PassedList, _FailedList}) ->
    ?log("Summary: ~p passed, ~p failed", [Passed, Failed]).


%% ---------------------------------------------------------------------
%% This function is used by aegis
%% 
%% It compiles the given Erlang module name
%% It run all the exported functions of arity 0 in it.
%% TODO: Make this more modular
erlfilename(Erlfile) ->
    Dir = filename:dirname(Erlfile),
    Module = filename:basename(Erlfile,".erl"),
    File = filename:join(Dir, Module),
    case compile:file(Erlfile, [return_errors, binary]) of
	{error, _ErrorList, _WarningList} ->
	    ?log("Cannot compile test module: ~s~n", [Erlfile]),
	    timer:sleep(10),
	    halt(1);
	{ok, ModuleName, Binary} ->
	    {module, ModuleName} = code:load_binary(ModuleName, File, Binary),
	    Exports = ModuleName:module_info(exports),
	    ?log("Running unit tests in module [~p]", [ModuleName]),
	    Result = lists:foldl(fun({module_info,0},Acc) ->
					 Acc;
				    ({Function, 0}, Acc) ->
					 case run_test(ModuleName, Function) of
					     passed -> Acc;
					     failed -> Acc + 1
					 end;
				    (_OtherArity, Acc) ->
					 Acc
				 end,
				 0,
				 Exports),
	    case Result of
		%% No test failed
		0 -> timer:sleep(10), halt();
		%% Some tests failed
		_Other ->
		    ?log("Failure.~n", []),
		    timer:sleep(10), halt(1)
	    end
    end.


%% Previously in eunit_lib.erl:

log(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Tag = lists:concat([File, "(", Line, ")"]),
    Data = io_lib:format(Format, Args),
    error_logger:info_report({Tag, lists:flatten(Data)}).
    
%% TODO: shouldn't this create a different report than log/4?
error(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Tag = lists:concat([File, "(", Line, ")"]),
    Data = io_lib:format(Format, Args),
    error_logger:info_report({Tag, lists:flatten(Data)}).
