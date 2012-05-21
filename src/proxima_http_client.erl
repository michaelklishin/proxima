-module(proxima_http_client).


-export([request/4]).


request(get, Url, Headers, _Req) ->
    httpc:request(get,
                  {Url, proxima_http:normalize_headers(Headers)},
                  [{relaxed, true}], []);
request(post, Url, Headers, Req) ->
    {ok, Body, _} = cowboy_http_req:body(Req),
    httpc:request(post, {Url,
                         proxima_http:normalize_headers(Headers),
                         proxima_http:as_list(proxima_http:post_content_type(Headers)),
                         proxima_http:as_list(Body)},
                  [], []);
request(Method, Url, Headers, _Req) ->
    httpc:request(Method,
                  {Url, proxima_http:normalize_headers(Headers)},
                  [{relaxed, true}], []).

