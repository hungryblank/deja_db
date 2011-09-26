-module(dj_db).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-define(BASE_PATH, "/tmp").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc: given a name initializes a db in /tmp with the given name
start_link(DbName) ->
    gen_server:start_link({local, DbName}, ?MODULE, [DbName], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(DbName) ->
    DbDir = filename:join([?BASE_PATH, DbName]),
    {ok, Ref} = eleveldb:open(DbDir, [{create_if_missing, true}]),
    State = Ref,
    {ok, State}.

handle_call({put, Key, Data}, _From, State) ->
    Reply = eleveldb:put(State, Key, Data, []),
    {reply, Reply, State};

handle_call({get, Key}, _From, State) ->
    Reply = eleveldb:get(State, Key, []),
    {reply, Reply, State};

handle_call({delete, Key}, _From, State) ->
    Reply = eleveldb:delete(State, Key, []),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

