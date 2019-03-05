
-module(shot_utils).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([
  to_binary/1,
  get_code/1,
  get_body/1,
  redirect/1,
  catch_redirect/1,
  http/1,
  multipart/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("shot.hrl").

%%% ==================================================================
%%% Public functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% httpc request without headers
%% @end
%% -------------------------------------------------------------------
-spec http(Params :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

http(#{m := post, u := URL}) -> httpc:request(post, {URL, [], [], []}, [], []);
http(#{m := Method, u := URL}) -> httpc:request(Method, {URL, []}, [?NO_REDIRECT], []).

%% -------------------------------------------------------------------
%% @doc
%% httpc request for upload file
%% #{m := method, u := "URL", p := "Path2File", o := Ops, cd := "Content-Disposition", ct := "Content-Type"}
%% @end
%% -------------------------------------------------------------------

multipart(#{m := M, u := U, p := P, o := O, cd := CD, ct := CT}) when M =:= post; M =:= put ->
  case file:read_file(P) of
    {ok, Bin} ->
      Data = binary_to_list(Bin),
      ReqBody = form_data(fname(P), Data, CD, CT),
      ContentType = lists:concat(["multipart/form-data; boundary=", ?BOUNDARY]),
      ReqHeader = [{"Content-Length", integer_to_list(length(ReqBody))}],
      httpc:request(M, {U, ReqHeader, ContentType, ReqBody}, O, []);
    _ -> {error, <<"Cannot find file">>}
  end;
multipart(_) -> {error, <<"Shot Error: The method not allowed">>}.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response code.
%% @end
%% -------------------------------------------------------------------
-spec get_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_code({{_, Code, _}, _, _}) -> Code.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response body.
%% @end
%% -------------------------------------------------------------------
-spec get_body(HttpcResult :: tuple()) -> Body :: list().

get_body({_, _, Body}) -> Body.

%% -------------------------------------------------------------------
%% @doc
%% Catch redirect and GET response from redirect URL
%% > {_, Resp1} = shot_utils:http(get, "http://google.com").
%% > {_, Resp2} = shot_utils:redirect(Resp).
%% @end
%% -------------------------------------------------------------------
-spec redirect(HttpcResult :: tuple()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

redirect({{_, 301, _}, Headers, _}) -> http(#{m => get, u => proplists:get_value("location", Headers)}).

%% -------------------------------------------------------------------
%% @doc
%% Catch redirect URL
%% @end
%% -------------------------------------------------------------------
-spec catch_redirect(HttpcResult :: tuple()) -> Result :: tuple() | list().

catch_redirect({{_, 301, _}, Headers, _} = Resp) -> proplists:get_value("location", Headers, Resp);
catch_redirect(Resp) -> Resp.

%% -------------------------------------------------------------------
%% @doc
%% Convert any to binary
%% @end
%% -------------------------------------------------------------------
-spec to_binary(Data) -> Result :: binary() when Data :: atom() | list() | binary().

to_binary(Data) when is_binary(Data)  -> Data;
to_binary(Data) when is_atom(Data)    -> list_to_binary(atom_to_list(Data));
to_binary(Data) when is_list(Data)    -> list_to_binary([to_binary(El) || El <- Data]);
to_binary(Data) when is_integer(Data) -> integer_to_binary(Data);
to_binary(Data) when is_tuple(Data)   -> to_binary([to_binary(El) || El <- tuple_to_list(Data)]);
to_binary(Data) when is_map(Data)     -> to_binary([to_binary(El) || El <- maps:to_list(Data)]).

%%% ==================================================================
%%% Internal/Private functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Prepare Multipart Form Data
%% @end
%% -------------------------------------------------------------------
-spec form_data(Fname :: list(), Data :: list(), CD :: list(), CT :: list()) -> Result :: string().

form_data(Fname, Data, CD, CT) ->
  string:join([
    lists:concat(["--", ?BOUNDARY]),
    lists:concat(["Content-Disposition: ", CD, ";", "name=", Fname]),
    lists:concat(["Content-Type: ", CT]),
    "",
    Data,
    lists:concat(["--", ?BOUNDARY, "--"])
  ], "\r\n").

%% -------------------------------------------------------------------
%% @private
%% @doc
%% Prepare Multipart Form Data
%% @end
%% -------------------------------------------------------------------
-spec fname(Path :: list()) -> Fname :: list().

fname(Path) ->
  L = binary:split(list_to_binary(Path), <<"/">>, [global]),
  [H|_] = lists:reverse(L),
  binary_to_list(H).
