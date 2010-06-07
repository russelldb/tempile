%%%-------------------------------------------------------------------
%%% File    : tempile.erl
%%% Author  : Russell Brown <russell@pango.lan>
%%% Description : 
%%%
%%% Created : 26 Feb 2010 by Russell Brown <russell@pango.lan>
%%%-------------------------------------------------------------------
-module(tempile).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

%% API
-export([start_link/0, render/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(EXT, ".mustache").

-record(state, {templates, dependencies, vars, root, extension, timers, last, dep_last}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, application:get_all_env(), []).

%%--------------------------------------------------------------------
%% Function: render() -> iolist()
%% View atom() the template
%% Context dict of name values to render
%% Description: render View with Context
%%--------------------------------------------------------------------
render(View, Context) when is_atom(View) ->
    gen_server:call(?SERVER, {render, View, Context});
render(View, Context) when is_list(View) ->
	render(list_to_atom(View), Context).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init(Args) ->
    error_logger:info_msg("Starting tempile with env ~p~n", [Args]),
    Root = proplists:get_value(root, Args),
    Ext = proplists:get_value(extension, Args, ?EXT),
	Vars = proplists:get_value(vars, Args, [{}]),
    Files = get_files(Root, Ext),
    Templates = check_and_compile(Files, Root, Vars, stamp(), 0,  dict:new()),
	Dependencies = get_dependencies(Templates),
    {ok, TRef} = timer:send_interval(timer:seconds(5), check_templates),
	{ok, DRef} = timer:send_interval(timer:seconds(5), check_deps),
    {ok, #state{templates=Templates, dependencies=Dependencies, vars=Vars, root=Root, extension=Ext, timers=[TRef, DRef], last=stamp(), dep_last=stamp()}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({render, View, Context}, _From, State) ->
    Dict = State#state.templates,
    case dict:find(View, Dict) of
		{ok, CompiledTemplate} ->
			error_logger:info_msg("Rendering ~p~n", [View]),
			Reply =  CompiledTemplate:render(Context);
		error ->
			Reply = {error, "Template " ++ atom_to_list(View) ++ " not found"}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(check_templates, State) ->
    Now = stamp(),
    Files = get_files(State#state.root, State#state.extension),
    Templates = check_and_compile(Files, State#state.root, State#state.vars, Now, State#state.last, State#state.templates),
	Deps = get_dependencies(Templates),
    {noreply, State#state{last=Now, templates=Templates, dependencies=Deps}};
handle_info(check_deps, State) ->
	Now = stamp(),
	Templates = check_deps(dict:to_list(State#state.dependencies), State#state.root, State#state.vars, Now, State#state.dep_last, State#state.templates),
	{noreply, State#state{dep_last=Now, templates=Templates}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
	cancel_timers(State#state.timers),
	ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
check_deps([], _, _, _, _, Templates) ->
	Templates;
check_deps([{Dep, Files}|T], Root, Vars, Now, Then, Templates) ->
	case file:read_file_info(Dep) of
		{ok, #file_info{mtime=Mtime}} when Mtime >= Then, Mtime < Now ->
			check_deps(T, Root, Vars, Now, Then, compile(Files, Root, Vars,  Templates));
		_ ->
			check_deps(T, Root, Vars, Now, Then, Templates)
	end.

compile([], _, _, Templates) ->
	Templates;
compile([File|T], Root, Vars, Templates) ->
	case compile_template(File, Root, Vars) of
		{ok, K, V} ->
			compile(T, Root, Vars,  dict:store(K, V, Templates));
		{error, _, _} ->
			%% TODO look at this, is it better to crash here?
			compile(T, Root, Vars, Templates)
	end.

cancel_timers([])->
	ok;
cancel_timers([H|T]) ->
	timer:cancel(H),
	cancel_timers(T).

compile_template(File, Root, Vars) ->
    error_logger:info_msg("Compiling ~p~n", [File]),
    View = list_to_atom(filename:basename(filename:rootname(File))),
    try erlydtl:compile(File, View, [{doc_root, Root}, {vars, Vars}]) of
		ok ->  {ok, View, View}
    catch
		ExType:Mess ->
			error_logger:error_msg("Failed compiling ~p with ~p:~p~n", [File, ExType, Mess]),
			{error, ExType, Mess}
    end.

check_and_compile([], _, _, _, _, Templates) ->
    Templates;
check_and_compile([File|T], Root, Vars, Now, Then, Templates) ->
    case file:read_file_info(File) of
		{ok, #file_info{mtime=Mtime}} when Mtime >= Then, Mtime < Now ->
			case compile_template(File, Root, Vars) of
				{ok, K, V} ->
				   	check_and_compile(T, Root, Vars, Now, Then,  dict:store(K, V, Templates));
				{error, _, _} ->
					%% Is it better to crash than have missing templates?
					check_and_compile(T, Root, Vars, Now, Then , Templates)
			end;
		{_, _} ->
			check_and_compile(T, Root, Vars, Now, Then , Templates)
    end.

stamp() ->
    erlang:localtime().

get_files(Root, Extension) ->
    tempile_find:files(Root, "*" ++ Extension, true).

%% Register the passed view against the dependancy so that we can recompile the view if any deps changes
get_dependencies(Views) ->
	get_dependencies(dict:fetch_keys(Views), dict:new()).

get_dependencies([], Deps) ->
	Deps;
get_dependencies([H|T], Deps) ->
	get_dependencies(T, reverse_register_dependencies(H, Deps)).
	
reverse_register_dependencies(View, Deps) ->
	{Source, _} = View:source(),
	register_dependency(View:dependencies(), Source, Deps).

register_dependency([], _, Deps) ->
	Deps;
register_dependency([{Dep, _}|T], Source, Deps) ->
	register_dependency(T, Source, dict:append(Dep, Source, Deps)).






