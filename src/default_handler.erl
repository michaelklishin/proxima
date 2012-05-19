-module(default_handler).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/2]).


%%
%% API
%%

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {Method, _} = cowboy_http_req:method(Req),
    {Path, _} = cowboy_http_req:path(Req),

    Id = mkid(Method, Path),
    Hash = riak_core_util:chash_key({Path, Id}),
    Index = case riak_core_apl:get_primary_apl(Hash, 1, proxima) of
                [{Idx, _Type}] -> Idx;
                _ -> {0, node()}
            end,
    lager:info("Dispatching to ~p", [Index]),

    case riak_core_vnode_master:sync_spawn_command(Index, {Method, Path, Req}, proxima_vnode_master) of
        {ok, R} -> R;
        {error, Reason} -> cowboy_http_req:reply(500, [], Reason, Req);
        Unhandled ->
            lager:warning("Unhandled reply: ~p~n", [Unhandled]),
            cowboy_http_req:reply(500, [], <<"Unhandled reply">>, Req)
    end,
    {ok, Req, State}.


terminate(_Req, _State) ->
    ok.


mkid(Method, Resource) ->
  % Absconded from riak_core_util:mkclientid/1
  {{Y,Mo,D},{H,Mi,S}} = erlang:universaltime(),
  {_,_,NowPart} = now(),
  Id = erlang:phash2([Y,Mo,D,H,Mi,S,Method,Resource,NowPart]),
  io_lib:format("~p", [Id]).
