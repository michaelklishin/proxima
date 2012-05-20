-module(default_handler).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


%%
%% API
%%

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, Req2} = cowboy_http_req:method(Req),
    {Path, Req3} = cowboy_http_req:path(Req2),

    Id = mkid(Method, Path),
    Hash = riak_core_util:chash_key({Path, Id}),
    Index = case riak_core_apl:get_primary_apl(Hash, 1, proxima) of
                [{Idx, _Type}] -> Idx;
                _ -> {0, node()}
            end,
    lager:debug("Dispatching to ~p", [Index]),

    Result = case riak_core_vnode_master:sync_spawn_command(Index, {Method, Path, Req3}, proxima_vnode_master) of
                 {ok, R} ->
                     R;
                 {error, Reason} ->
                     {ok, R} = cowboy_http_req:reply(500, [], Reason, Req3),
                     R;
                 Unhandled ->
                     lager:warning("Unhandled reply: ~p~n", [Unhandled]),
                     {ok, R} = cowboy_http_req:reply(500, [], <<"Unhandled reply">>, Req3),
                     R
             end,
    {_N, Name} = Index,
    {Url, UpstreamStatus, UpstreamHeaders, UpstreamBody} = Result,
    case UpstreamStatus of
        400 ->
            lager:warning("Response is 400, URL: ~p", [Url]);
        401 ->
            lager:warning("Response is 401, URL: ~p", [Url]);
        _ ->
            ok
    end,

    {ok, Res} = cowboy_http_req:reply(UpstreamStatus,
                                      normalize_headers(UpstreamHeaders) ++
                                          [{<<"X-Handling-Node">>, atom_to_list(Name)}],
                                      UpstreamBody,
                                      Req3),
    {ok, Res, State}.


terminate(_Req, _State) ->
    ok.


mkid(Method, Resource) ->
    {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
    {_,_,NowPart} = now(),
    Id = erlang:phash2([Y,Mo,D,H,Mi,S,Method,Resource,NowPart]),
    io_lib:format("~p", [Id]).

normalize_headers(L) ->
    [{list_to_binary(capitalize(K)), list_to_binary(V)} || {K, V} <- L].

capitalize([F|Rest]) ->
    [string:to_upper(F) | string:to_lower(Rest)].
