-module(dj_web).
-include_lib("cowboy/include/http.hrl").
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

%% cowboy handling callback
handle(Req, State) ->
    {ok, Reply} = handle_method_and_path(Req#http_req.method,
                                   Req#http_req.path,
                                   Req),
    {ok, Reply, State}.

%% create a db
handle_method_and_path('PUT', [DbName], Req) ->
    {ok, _Pid} = dj_db:start_link(db(DbName)),
    cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain">>}], ["db ", DbName, " created"], Req);

%% get a key from a db
handle_method_and_path('GET', [DbName, Key], Req) ->
    case gen_server:call(db(DbName), {get, Key}) of
        {ok, Value} ->
            cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain">>}], [Value], Req);
        not_found ->
            cowboy_http_req:reply(404, [{<<"Content-Type">>, <<"text/plain">>}], ["key not found"], Req)
    end;

%% put a key into the db
handle_method_and_path('PUT', [DbName, Key], Req) ->
    {ok, Body, _Req} = cowboy_http_req:body(Req),
    ok = gen_server:call(db(DbName), {put, Key, Body}),
    cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/plain">>}], ["key ", Key, " updated"], Req);

handle_method_and_path(Verb, Path, Req) ->
    error_logger:info_msg("unknown request verb ~p path ~p Req ~p ~n", [Verb, Path, Req]),
    cowboy_http_req:reply(404, [{<<"Content-Type">>, <<"text/plain">>}], ["unknown request"], Req).

terminate(_Req, _State) ->
    ok.

%% helper

db(BinaryName) ->
    list_to_atom(binary_to_list(BinaryName)).
