# Proxima

This is an experimental Dynamo-style application based on [Riak Core](https://github.com/basho/riak_core). It is not yet
ready to be announced or used by anybody except the author. It also lacks automated tests. So please, ignore it
for now.


## Generating a release

### Development

    # same as rebar compile generate overlay_vars=development.vars.config
    rm -rf rel/proxima && rebar compile generate


### Production

    rm -rf rel/proxima && rebar compile generate overlay_vars=production.vars.config


## License

Copyright (C) 2012 Michael S. Klishin, Alex Petrov

Dual-licensed under the [Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html) or the [Apache 2.0 License](http://www.apache.org/licenses/LICENSE-2.0.html).
