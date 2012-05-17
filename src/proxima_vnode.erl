-module(proxima_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
  delete/1,
  encode_handoff_item/2,
  handle_command/3,
  handle_coverage/4,
  handle_exit/3,
  handle_handoff_command/3,
  handle_handoff_data/2,
  handoff_cancelled/1,
  handoff_finished/2,
  handoff_starting/2,
  init/1,
  is_empty/1,
  start_vnode/1,
  terminate/2
]).

-record(state, {
  partition :: partition()
               %% , client
}).

start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  lager:debug("started ~p at partition ~p", [?MODULE, Partition]),
  {ok, Client} = cowboy_client:init([]),
  {ok, #state {partition = Partition
              %% , client = Client
              }}.

handle_command({'GET', _Path, Req}, _Sender, State) ->
  {Host, _} = cowboy_http_req:header('Host', Req),
  lager:info("Host: ~p", [binary_to_list(Host)]),
  {ok, UpstreamStatus, UpstreamHeaders, UpstreamBody} = ibrowse:send_req("http://" ++ binary_to_list(Host), [], get),
  lager:info("GET ~p => ~p~n", [("http://" ++ binary_to_list(Host)), UpstreamStatus]),
  {ok, R} = cowboy_http_req:reply(200, [], <<"I am a cowboy">>, Req),
  {reply, {ok, R}, State};

handle_command({Method, Path, Req}, _Sender, State) ->
  lager:warning("unhandled command: ~p~n", [{Method, Path, Req}]),
  {ok, R} = cowboy_http_req:reply(405, [{<<"allow">>, <<"GET">>}], <<"Hello from Cowboy!">>, Req),
  {reply, {ok, R}, State}.

handle_coverage(Request, KeySpaces, Sender, State) ->
  lager:debug("handle_coverage: ~p ~p ~p ~p~n", [Request, KeySpaces, Sender, State]),
  {continue, State}.

handle_exit(Pid, Reason, State) ->
  lager:debug("handle exit: ~p ~p~n", [Pid, Reason]),
  {stop, Reason, State}.

handle_handoff_command(Message, Sender, State) ->
  lager:debug("handle handoff: ~p ~p~n", [Message, Sender]),
  {forward, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_Data, State) ->
  {reply, ok, State}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
  <<>>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
