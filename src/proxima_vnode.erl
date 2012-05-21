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
    Method2 = proxima_http:normalize_method(Method),
    Url = upstream_url(Req, Path),
    case proxima_http_client:request(Method2, Url, Headers, Req) of
        {ok, {{_, UpstreamStatus, _}, UpstreamHeaders, UpstreamBody}} ->
            lager:info("~p ~p => ~p~n", [Method2, Url, UpstreamStatus]),
            {reply, {ok, {Url, UpstreamStatus, UpstreamHeaders, UpstreamBody}}, State};
        {error, {connect_failed, T}} ->
            {reply, {error, <<"Connection failed">>}, State};
        {error, {send_failed, T}} ->
            {reply, {error, <<"Send failed">>}, State};
        {error, _} ->
            {reply, {error, <<"Unexpected failure">>}, State}
    end.


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
            Path ++ ?QS_SEPARATOR
                ++ binary_to_list(QS)
    end.
