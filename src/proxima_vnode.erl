-module(proxima_vnode).
-behaviour(riak_core_vnode).

-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(SLASH, "/").
-define(QS_SEPARATOR, "?").

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
}).

start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
  lager:debug("started ~p at partition ~p", [?MODULE, Partition]),
  {ok, #state {partition = Partition}}.



handle_command({Method, Path, Req}, _Sender, State) ->
  {Headers, _} = cowboy_http_req:headers(Req),
  Url = upstream_url(Req, Path),
  {ok, {{_, UpstreamStatus, _}, UpstreamHeaders, UpstreamBody}} = httpc:request(get, {Url, normalize_headers(Headers)}, [{relaxed, true}], []),
  lager:info("GET ~p => ~p~n", [Url, UpstreamStatus]),
  {reply, {ok, {Url, UpstreamStatus, UpstreamHeaders, UpstreamBody}}, State}.



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


upstream_url(Req, PathSegments) ->
    {Host, _} = cowboy_http_req:header('Host', Req),
    "http://" ++ binary_to_list(Host) ++ path_from(Req, PathSegments).

path_from(Req, PathSegments) ->
    {QS, _} = cowboy_http_req:raw_qs(Req),
    Path = ?SLASH ++
        string:join(lists:map(fun erlang:binary_to_list/1, PathSegments), ?SLASH),
    case QS of
        [] ->
            Path;
        <<>> ->
            Path;
        V ->
            lager:info(V),
            Path ++ ?QS_SEPARATOR
                ++ binary_to_list(QS)
    end.


as_list(K) when is_list(K) ->
    k;
as_list(K) when is_atom(K) ->
    atom_to_list(K);
as_list(K) when is_binary(K) ->
    binary_to_list(K).

normalize_method(<<"GET">>) ->
    get;
normalize_method("GET") ->
    get;
normalize_method(<<"get">>) ->
    get;
normalize_method("get") ->
    get;
normalize_method(get) ->
    get;
normalize_method(Method) ->
    list_to_binary(atom_to_list(Method)).

normalize_headers(L) ->
    [{capitalize(as_list(K)), binary_to_list(V)} || {K, V} <- L].

capitalize([F|Rest]) ->
    [string:to_upper(F) | string:to_lower(Rest)].
