-module(proxima_sup).
-behaviour(supervisor).

-include("proxima.hrl").

-export([
  start_link/0,
  init/1
]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  Workers = [
    ?GEN_SERVER(proxima_vnode_dispatcher_master, proxima_vnode_dispatcher),
    ?VNODE_MASTER(proxima_vnode_master, proxima_vnode)
  ],
  
  {ok, {{one_for_one, 10, 10}, Workers}}.
