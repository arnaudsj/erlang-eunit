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
%% File: eunit_data.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Interpretation of symbolic test representation

-module(eunit_data).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([list/1, iter_init/1, iter_next/2, iter_prev/2,
	 enter_context/3, browse_context/2]).

-import(lists, [foldr/3]).


%% @type tests() =
%%            SimpleTest
%%          | [tests()]
%%          | moduleName()
%%          | {string(), tests()}
%%          | {string(), term(), term()}
%%          | {string(), term(), term(), term()}
%%          | {generator, () -> tests()}
%%          | {generator, M::moduleName(), F::functionName()}
%%          | {spawn, tests()}
%%          | {inorder, tests()}
%%          | {inparallel, tests()}
%%          | {setup, Setup::() -> R::any(),
%%                    Cleanup::(R::any()) -> any(),
%%                    Instantiate::(R::any()) -> tests()
%%            }
%%          | {foreach, Setup::() -> R::any(),
%%                      Cleanup::(R::any()) -> any(),
%%                      Instantiators::[(R::any()) -> tests()]
%%            }
%%          | {foreach1, Setup::(A::any()) -> R::any(),
%%                       Cleanup::(A::any(), R::any()) -> any(),
%%                       Pairs::[{A::any(),
%%                               (A::any(), R::any()) -> tests()}]
%%            }
%%
%% SimpleTest = TestFunction | {Line::integer(), TestFunction}
%%
%% TestFunction = () -> any()
%%              | {M::moduleName(), F::functionName()}.
%%
%% Note that `{string(), ...}' is equivalent to `{string(), {...}}' if
%% the tuple contains more than two elements.
%%
%% @type moduleName() = eunit_lib:moduleName()
%% @type functionName() = eunit_lib:functionName()

%% ---------------------------------------------------------------------
%% Abstract test set iterator

-record(iter,
	{prev = [],
	 next = [],
	 tests = []}).

%% @spec (tests()) -> testIterator()
%% @type testIterator()

iter_init(Tests) ->
    #iter{tests = Tests}.

%% @throws {bad_test, term()}
%%       | {generator_failed, exception()}
%%       | {no_such_function, eunit_lib:mfa()}
%%       | {module_not_found, moduleName()}

%% @spec (testIterator(), Handler) -> none | {testItem(), testIterator()}
%%    Handler = (term()) -> term()

iter_next(I, H) ->
    iter_do(fun iter_next/1, I, H).

iter_do(F, I, H) ->
    try F(I)
    catch
	R = {bad_test, _Bad} ->
	    H(R);
	R = {no_such_function, _MFA} ->
	    H(R);
	R = {module_not_found, _M} ->
	    H(R);
	R = {generator_failed, _Exception} ->
	    H(R)
    end.

iter_next(I = #iter{next = []}) ->
    case next(I#iter.tests) of
	{T, Tests} ->
	    {T, I#iter{prev = [T | I#iter.prev],
		       tests = Tests}};
	none ->
	    none
    end;
iter_next(I = #iter{next = [T | Ts]}) ->
    {T, I#iter{next = Ts,
	       prev = [T | I#iter.prev]}}.


%% @spec (testIterator(), Handler) -> none | {testItem(), testIterator()}
%%    Handler = (term()) -> term()

iter_prev(I, H) ->
    iter_do(fun iter_prev/1, I, H).

iter_prev(#iter{prev = []}) ->
    none;
iter_prev(#iter{prev = [T | Ts]} = I) ->
    {T, I#iter{prev = Ts,
	       next = [T | I#iter.next]}}.


%% ---------------------------------------------------------------------
%% Concrete test set representation iterator

%% @spec (tests()) -> none | {testItem(), tests()}
%% @type testItem() = #test{} | #group{}
%% 
%% @throws {bad_test, term()}
%%       | {generator_failed, eunit_lib:exception()}
%%       | {no_such_function, eunit_lib:mfa()}
%%       | {module_not_found, moduleName()}

next(Tests) ->
    case eunit_lib:dlist_next(Tests) of
	[T | Ts] ->
	    case parse(T) of
		Ts1 when is_list(Ts1) ->
		    next([Ts1 | Ts]);
		T1 ->
		    {T1, Ts}
	    end;
	[] ->
	    none
    end.

parse({foreach, S, C, Fs} = T)
  when is_function(S), is_function(C), is_list(Fs) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    case eunit_lib:dlist_next(Fs) of
	[F | Fs1] when is_function(F) ->
	    check_arity(F, 1, T),
	    [{setup, S, C, F} | {foreach, S, C, Fs1}];
	[] ->
	    [];
	_ ->
	    throw({bad_test, T})
    end;
parse({foreach1, S1, C1, Ps} = T) 
  when is_function(S1), is_function(C1), is_list(Ps) ->
    check_arity(S1, 1, T),
    check_arity(C1, 2, T),
    case eunit_lib:dlist_next(Ps) of
	[P | Ps1] ->
	    case P of
		{A, F1} when is_function(F1) ->
		    check_arity(F1, 2, T),
		    S = fun () -> S1(A) end,
		    C = fun (X) -> C1(A, X) end,
		    F = fun (X) -> F1(A, X) end,
		    [{setup, S, C, F} | {foreach1, S1, C1, Ps1}];
		_ ->
		    throw({bad_test, T})
	    end;
	[] ->
	    []
    end;
parse({generator, F} = T) when is_function(F) ->
    check_arity(F, 0, T),
    %% use run_testfun/1 to handle wrapper exceptions
    case eunit_test:run_testfun(F) of
	{ok, T1} ->
	    parse(T1);
	{error, {Class, Reason, Trace}} ->
	    throw({generator_failed, {Class, Reason, Trace}})
    end;
parse({generator, M, F}) when is_atom(M), is_atom(F) ->
    parse({generator, eunit_test:function_wrapper(M, F)});
parse({inorder, T}) ->
    group(#group{tests = T, order = true});
parse({inparallel, T}) ->
    group(#group{tests = T, order = false});
parse({spawn, T}) ->
    group(#group{tests = T, spawn = true});
parse({setup, S, C, I} = T)
  when is_function(S), is_function(C), is_function(I) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    check_arity(I, 1, T),
    %% note that the test is nonstandard - it needs instantiating
    group(#group{tests = I, context = #context{setup = S, cleanup = C}});
parse({S, T1} = T) when is_list(S) ->
    case eunit_lib:is_string(S) of
	true ->
	    group(#group{tests = T1, desc = S});
	false ->
	    throw({bad_test, T})
    end;
parse(T) when is_tuple(T), size(T) > 2, is_list(element(1, T)) ->
    [S | Es] = tuple_to_list(T),
    parse({S, list_to_tuple(Es)});
parse(M) when is_atom(M) ->
    get_module_tests(M);
parse(T) when is_list(T) ->
    T;
parse(T) ->
    parse_simple(T).

parse_simple({L, F}) when is_integer(L), L >= 0 ->
    (parse_simple(F))#test{line = L};
parse_simple(F) ->
    parse_function(F).

parse_function(F) when is_function(F) ->
    check_arity(F, 0, F),
    {module, M} = erlang:fun_info(F, module),
    #test{f = F, module = M, name = eunit_lib:fun_parent(F)};
parse_function({M,F}) when is_atom(M), is_atom(F) ->
    #test{f = eunit_test:function_wrapper(M, F), module = M, name = F};
parse_function(F) ->
    throw({bad_test, F}).

check_arity(F, N, T) ->
    case erlang:fun_info(F, arity) of
	{arity, N} ->
	    ok;
	_ ->
	    throw({bad_test, T})
    end.

%% This does some look-ahead and folds nested groups and tests where
%% possible. E.g., {String, Test} -> Test#test{desc = String}.

group(#group{context = #context{}} = G) ->
    %% leave as it is - the test body is not suitable for lookahead
    G;
group(#group{tests = T0, desc = Desc, order = Order, context = Context,
	     spawn = Spawn} = G) ->
    {T1, Ts} = lookahead(T0),
    {T2, _} = lookahead(Ts),
    case T1 of
	#test{desc = undefined}
	when T2 == none, Spawn /= true, Context == undefined ->
	    %% a single test within a non-spawn/setup group: put the
	    %% description directly on the test; no order.
	    T1#test{desc = Desc};
	#group{desc = Desc1, order = Order1, context = Context1,
	       spawn = Spawn1}
	when T2 == none,
	     ((Desc == undefined) or (Desc1 == undefined)),
	     ((Order == undefined) or (Order1 == undefined)),
	     ((Context == undefined) or (Context1 == undefined)),
	     ((Spawn == undefined) or (Spawn1 == undefined)) ->
	    %% two nested groups with non-conflicting properties
	    T1#group{desc = join_properties(Desc, Desc1),
		     order = join_properties(Order, Order1),
		     context = join_properties(Context, Context1),
		     spawn = join_properties(Spawn, Spawn1)};
	_ ->
	    %% leave as it is and discard the lookahead
	    G
    end.

lookahead(T) ->
    case next(T) of
	{T1, Ts} -> {T1, Ts};
	none -> {none, []}
    end.

join_properties(undefined, X) -> X;    
join_properties(X, undefined) -> X.


%% ---------------------------------------------------------------------
%% Extracting test funs from a module

%% @throws {module_not_found, moduleName()}

get_module_tests(M) ->
    TestSuffix = ?DEFAULT_TEST_SUFFIX,
    GeneratorSuffix = ?DEFAULT_GENERATOR_SUFFIX,
    try M:module_info(exports) of
	Es ->
	    foldr(fun ({F, 0}, Fs) ->
			  N = atom_to_list(F),
			  case lists:suffix(TestSuffix, N) of
			      true ->
				  [{M,F} | Fs];
			      false ->
				  case lists:suffix(GeneratorSuffix, N) of
				      true ->
					  [{generator, M, F} | Fs];
				      false ->
					  Fs
				  end
			  end;
		      (_, Fs) ->
			  Fs
		  end,
		  [],
		  Es)
    catch
	error:undef -> 
	    throw({module_not_found, M})
    end.

%% ---------------------------------------------------------------------
%% Entering a setup-context, with guaranteed cleanup.

%% @spec (Tests::#context{}, Callback::(any()) -> any()) -> any()
%% @throws setup_failed | instantiation_failed | cleanup_failed

enter_context(#context{setup = S, cleanup = C}, I, F) ->
    enter_context(S, C, I, F).

enter_context(Setup, Cleanup, Instantiate, Callback) ->
    try Setup() of
	R ->
	    try Instantiate(R) of
		T ->
		    try Callback(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try Cleanup(R)
			catch
			    _:_ -> throw(cleanup_failed)
			end
		    end
	    catch
		_:_ ->
		    throw(instantiation_failed)
	    end
    catch
	_:_ ->
	    throw(setup_failed)
    end.

%% Instantiates a context with dummy values to make browsing possible
%% @throws instantiation_failed

browse_context(I, F) ->
    %% Browse: dummy setup/cleanup and a wrapper for the instantiator
    S = fun () -> ok end,
    C = fun (_) -> ok end,
    I1 = fun (_) ->
		try eunit_lib:browse_fun(I) of
		    {ok, _, T} ->
			T;
		    error ->
			throw(instantiation_failed)
		catch
		    _:_ ->
			throw(instantiation_failed)
		end
	 end,
    enter_context(S, C, I1, F).


%% ---------------------------------------------------------------------

%% Returns a list of test info using a similar format to tests() above:
%%
%% @type testInfoList() = [{Id, testInfo()}]
%%   Id = [integer()]
%% @type testInfo() = {moduleName(), functionName()}
%%		    | {moduleName(), functionName(), lineNumber()}
%%		    | {description(), testInfo()}
%%		    | {description(), testInfoList()}
%% @type lineNumber() = integer().  Line numbers are always >= 1.

list(T) ->
    try list(T, 1, [])
    catch
	{error, R} -> {error, R}
    end.

list(T, N, Ns) ->
    list_loop(iter_init(T), N, Ns).

list_loop(I, N, Ns) ->
    case iter_next(I, fun (R) -> throw({error, R}) end) of
 	{T, I1} ->
	    Id = id(N, Ns),
 	    case T of
		#test{} ->
		    Name = case T#test.line of
			       0 -> {T#test.module, T#test.name};
			       Line -> {T#test.module, T#test.name, Line}
			   end,
		    [{item, Id, desc_string(T#test.desc), Name}
		     | list_loop(I1, N + 1, Ns)];
		#group{context = #context{}} ->
		    [{group, Id, desc_string(T#group.desc),
		      list_context(T#group.tests, 1, [N | Ns])}
		     | list_loop(I1, N + 1, Ns)];
		#group{} ->
		    [{group, Id, desc_string(T#group.desc),
		      list(T#group.tests, 1, [N | Ns])}
		     | list_loop(I1, N + 1, Ns)]
	    end;
 	none ->
 	    []
    end.

desc_string(undefined) -> "";
desc_string(S) -> S.

id(N, Ns) ->
    lists:reverse(Ns, [N]).

list_context(T, N, Ns) ->
    try
 	browse_context(T, fun (T) -> list(T, N, Ns) end)
    catch
 	R = instantiation_failed ->
 	    {error, R}
    end.
