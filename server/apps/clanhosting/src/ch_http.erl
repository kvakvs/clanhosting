%%%_* Module declaration =======================================================
-module(ch_http).

%%%_* Exports ==================================================================
%% HTTP
-export([ get/1
        , get/2
        , post/2
        , post_multi_part/3
        , post_multi_part/4
        ]).

%% Returned state. Utilities
-export([ body/1
        , cookie/1
        , headers/1
        , status/1
        ]).

% Misc utilities
-export([ format_url/2
        , parse_result/1
        , proplist_to_query_string/1
        ]).

%%%_* Defines ==================================================================

-record(state, { body = []
               , cookie = []
               , headers = []
               , status = 200
               }).

-type state() :: #state{}.
-type multipart_fields() :: [{atom(), any()}].
-type multipart_files() :: [{file, Filename::string(), Data::list()}].

%%%_* API  =====================================================================
-spec post(Url::string(), Data::proplists:proplist()) ->
              {ok, NewState::state()} | {error, Error::any()}.
post(Url, Data) ->
  Result = do_post(Url, Data),
  parse_result(Result).

%% @doc Post a multi part post with default boundary.
%% Code from http://lethain.com/formatting-multipart-formdata-in-erlang/ with
%% small changes to get it working.
%% @end
-spec post_multi_part(string(), multipart_fields(), multipart_files()) ->
                         {ok, state()} | {error, any()}.
post_multi_part(Url, Fields, Files) ->
  %% Random string that work fine as long as it doesn't collide with the data
  Boundary = "------------a450glvjfEoqerAc1p431paQlfDac152cadADfd",
  post_multi_part(Url, Fields, Files, Boundary).


%% @doc Post a multi part post with specified boundary.
%% @end
-spec post_multi_part(string(), multipart_fields(), multipart_fields(),
                      string()) -> {ok, state()} | {error, any()}.
post_multi_part(Url, Fields, Files, Boundary) ->
  Body = format_multipart_formdata( Boundary
                                  , Fields
                                  , Files
                                  ),
  ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
  Headers = [ {"Content-Length", integer_to_list(length(Body))}
            , [httpc:cookie_header(Url)]
            ],
  parse_result(httpc:request(post, {Url, Headers, ContentType, Body}, [], [])).

-spec get(Url::string()) ->
              {ok, NewState::state()} | {error, Error::any()}.
get(Url) ->
  Result = do_get(Url, []),
  parse_result(Result).

get(Url, HTTPOpts) ->
  Result = do_get(Url, HTTPOpts),
  parse_result(Result).

-spec cookie(state()) -> string().
cookie(#state{cookie = Cookie}) ->
  Cookie.

-spec body(state()) -> string().
body(#state{body = Response}) ->
  Response.

-spec status(state()) -> integer().
status(#state{status = Status}) ->
  Status.

-spec headers(state()) -> string().
headers(#state{headers = Headers}) ->
  Headers.

%%%_* Utilities=================================================================

%%
%% @doc Convert [{param1, value1}, {param2, value2}] to
%%      "param1=value1&amp;param2=value2"
%%      Will url encode both params and values in the process
%%
-spec proplist_to_query_string(proplists:proplist()) -> string().
proplist_to_query_string(List0) ->
  F = fun http_uri:encode/1,
  List = [lists:flatten([F(Key), "=", F(Value)]) || {Key, Value} <- List0],
  string:join(List, "&").

-spec format_url(string(), proplists:proplist()) -> string().
format_url(Url, QueryParams) ->
  Qs = proplist_to_query_string(QueryParams),
  lists:flatten([Url, "?", Qs]).

%%%_* Internal =================================================================

do_post(Url, Data) ->
  httpc:set_options([{cookies, enabled}]),
  httpc:request( post
               , { Url
                 , [httpc:cookie_header(Url)]
                 , "application/x-www-form-urlencoded"
                 , proplist_to_query_string(Data)
                 }
               , []
               , []).

do_get(Url, HTTPOpts) ->
  httpc:set_options([{cookies, enabled}]),
  httpc:request( get
               , { Url
                 , [httpc:cookie_header(Url)]
                 }
               , HTTPOpts
               , []).

parse_result({error, _} = Error) ->
  Error;
parse_result({ok, {Status, Headers, Body}}) ->
  Cookie = get_cookie_from_headers(Headers),
  {ok, #state{ cookie  = Cookie
             , headers = Headers
             , body    = Body
             , status  = Status
             }
  }.

format_multipart_formdata(Boundary, Fields, Files) ->
  FieldParts =
    lists:map(fun({FieldName, FieldContent}) ->
                   [lists:concat(["--", Boundary]),
                    lists:concat([ "Content-Disposition: form-data; name=\""
                                 , atom_to_list(FieldName)
                                 , "\""
                                 ]),
                    "",
                    FieldContent]
              end, Fields),
  FieldParts2 = lists:append(FieldParts),
  FileParts =
    lists:map(fun({FieldName, FileName, FileContent}) ->
                   [lists:concat(["--", Boundary]),
                    lists:concat(["Content-Disposition: form-data; name=\""
                                 , atom_to_list(FieldName)
                                 , "\"; filename=\""
                                 , FileName
                                 , "\""
                                 ]),
                    lists:concat([ "Content-Type: "
                                 , "application/octet-stream"
                                 ]),
                    "",
                    FileContent]
              end, Files),
  FileParts2 = lists:append(FileParts),
  EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
  Parts = lists:append([FieldParts2, FileParts2, EndingParts]),
  string:join(Parts, "\r\n").
%%%_* Helpers ==================================================================

get_cookie_from_headers(List) ->
  proplists:get_value("set-cookie", List, "").
