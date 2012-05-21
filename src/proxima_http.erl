-module(proxima_http).


-export([normalize_method/1,
         normalize_headers/1,
         post_content_type/1,
        as_list/1,
        capitalize/1]).


%%
%% API
%%

%% GET
normalize_method(<<"GET">>) ->
    get;
normalize_method("GET") ->
    get;
normalize_method('GET') ->
    get;
normalize_method(<<"get">>) ->
    get;
normalize_method("get") ->
    get;
normalize_method(get) ->
    get;
normalize_method(<<71,69,84>>) ->
    get;

%% POST
normalize_method(<<"POST">>) ->
    post;
normalize_method("POST") ->
    post;
normalize_method('POST') ->
    post;
normalize_method(<<"post">>) ->
    post;
normalize_method("post") ->
    post;
normalize_method(post) ->
    post;


normalize_method(Method) when is_atom(Method) ->
    list_to_atom(string:to_lower(atom_to_list(Method)));
normalize_method(Method) when is_binary(Method) ->
    list_to_atom(string:to_lower(binary_to_list(Method))).


normalize_headers(L) ->
    [{capitalize(as_list(K)), binary_to_list(V)} || {K, V} <- L].


post_content_type(Headers) ->
    case lists:keyfind('Content-Type', 1, Headers) of
        {'Content-Type', V} ->
            V;
        false ->
            <<"application/x-www-form-urlencoded">>
    end.



as_list(K) when is_list(K) ->
    k;
as_list(K) when is_atom(K) ->
    atom_to_list(K);
as_list(K) when is_binary(K) ->
    binary_to_list(K).


capitalize([F|Rest]) ->
    [string:to_upper(F) | string:to_lower(Rest)].


