-module(proxima_app).
-behaviour(application).

-export([
  start/2, 
  stop/1
]).

start(_StartType, _StartArgs) ->
  case proxima_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register([{vnode_module, proxima_vnode}]),
      ok = riak_core_node_watcher:service_up(proxima, self()),
      
      {ok, Pid};
    Else -> Else
  end.

stop(_State) ->
  ok.
