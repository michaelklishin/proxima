{application,proxima,
             [{description,"Experimental elastic HTTP proxy"},
              {vsn,"1.0"},
              {registered,[]},
              {applications,[kernel,stdlib,riak_core]},
              {mod,{proxima_app,[]}},
              {env,[]},
              {modules,[proxima,proxima_app,proxima_sup,proxima_vnode,
                        proxima_vnode_dispatcher]}]}.
