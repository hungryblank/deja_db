-module(cowboy_srv).
-export([start/1, stop/0]).

start(Port) ->
  application:start(cowboy),
  Dispatch = [
    %% {Host, list({Path, Handler, Opts})}
    {'_', [{'_', dj_web, []}]}
  ],
  %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
  cowboy:start_listener(http, 100,
    cowboy_tcp_transport, [{port, Port}],
    cowboy_http_protocol, [{dispatch, Dispatch}]
  ).

stop() ->
  application:stop(cowboy).
