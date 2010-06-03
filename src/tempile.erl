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

-record(state, {templates, root, extension, timer, last}).

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
    gen_server:call(?SERVER, {render, View, Context}).

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
    Files = get_files(Root, Ext),
    Dict = check_and_compile(Files, Root, stamp(), 0,  dict:new()),
    {ok, TRef} = timer:send_interval(timer:seconds(1), check_templates),
    {ok, #state{templates=Dict, root=Root, extension=Ext, timer=TRef, last=stamp()}}.

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
    Templates = check_and_compile(Files, State#state.root, Now, State#state.last, State#state.templates),
    {noreply, State#state{last=Now, templates=Templates}};
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
    {ok, cancel} = timer:cancel(State#state.timer),
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
compile_template(File, Root) ->
    error_logger:info_msg("Compiling ~p~n", [File]),
    View = list_to_atom(filename:basename(filename:rootname(File))),
    try erlydtl:compile(File, View, [{doc_root, Root}]) of
		ok ->  {ok, View, View}
    catch
		ExType:Mess ->
			error_logger:error_msg("Failed compiling ~p with ~p:~p~n", [File, ExType, Mess]),
			{error, ExType, Mess}
    end.

check_and_compile([], _, _, _, Templates) ->
    Templates;
check_and_compile([File|T], Root, Now, Then, Templates) ->
    case file:read_file_info(File) of
		{ok, #file_info{mtime=Mtime}} when Mtime >= Then, Mtime < Now ->
			case compile_template(File, Root) of
				{ok, K, V} ->
					check_and_compile(T, Root, Now, Then, dict:store(K, V, Templates));
				{error, _, _} ->
					check_and_compile(T, Root, Now, Then , Templates)
			end;
		{_, _} ->
			check_and_compile(T, Root, Now, Then , Templates)
    end.

stamp() ->
    erlang:localtime().

get_files(Root, Extension) ->
    tempile_find:files(Root, "*" ++ Extension, true).
    
