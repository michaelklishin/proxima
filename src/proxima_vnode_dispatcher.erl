-module(proxima_vnode_dispatcher).
-behaviour(gen_server2).

-include("proxima.hrl").

-export([
  start_link/0,
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-record(state, {site, server, config}).

start_link() ->
  gen_server2:start_link({local, ?MODULE}, ?MODULE, [], [{timeout, infinity}]).

init([]) ->
  inets:start(),
  application:start(crypto),
  application:start(public_key),
  application:start(ssl),
  {ok, MasterNode} = application:get_env(master_node),

  case node() of
    MasterNode -> ok;
    _ -> riak_core:join(MasterNode)
  end,

  WebPort = case application:get_env(web_port) of
    {ok, P} -> P;
    _ -> 4629
  end,

  lager:info("Starting HTTP server on ~p", [WebPort]),
  Dispatch = [{'_', [{'_', default_handler, []}]}],
  {ok, ServerPid} = cowboy:start_listener(just_http_listener, 100,
                        cowboy_tcp_transport, [{port, WebPort}],
                        cowboy_http_protocol, [{dispatch, Dispatch}]),

  {ok, #state {server = ServerPid}}.

handle_call(Msg, From, State) ->
  io:format("handle_call: ~p ~p ~p~n", [Msg, From, State]),
  {noreply, State}.

handle_cast(Msg, State) ->
  io:format("handle_cast: ~p ~p~n", [Msg, State]),
  {noreply, State}.

handle_info(Msg, State) ->
  io:format("handle_info: ~p ~p~n", [Msg, State]),
  {noreply, State}.

terminate(Reason, State) ->
  io:format("terminate: ~p ~p~n", [Reason, State]),
  ok.

code_change(OldVsn, State, Extra) ->
  io:format("code_change: ~p ~p ~p~n", [OldVsn, State, Extra]),
  {ok, State}.
