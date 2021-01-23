%% MIT License

%% Copyright (c) 2021 Viacheslav Katsuba <v.katsuba.dev@gmail.com>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.

-module(shot_utils).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([
    get_code/1,
    get_body/1,
    redirect/1,
    catch_redirect/1,
    http/1,
    multipart/1,
    httpc_request/1
]).

%%% ==================================================================
%%% Macros
%%% ==================================================================

-define(BOUNDARY, "----WebKitFormBoundaryShot").
-define(NO_REDIRECT, {autoredirect, false}).

%%% ==================================================================
%%% Public functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% httpc request with/without headers
%% @end
%% -------------------------------------------------------------------
-spec http(Params :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

http(#{m := _, u := _, b := _, ct := _} = Data) ->
    httpc_request(Data);
http(#{m := _, u := _, h := _} = Data) ->
    httpc_request(Data);
http(#{m := post, u := URL}) ->
    httpc:request(post, {URL, [], [], []}, [], []);
http(#{m := Method, u := URL}) ->
    httpc:request(Method, {URL, []}, [?NO_REDIRECT], []).

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
        _ ->
            {error, <<"Cannot find file">>}
    end;
multipart(_) ->
    {error, <<"Shot Error: The method not allowed">>}.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response code.
%% @end
%% -------------------------------------------------------------------
-spec get_code(HttpcResult :: tuple()) -> Code :: pos_integer().

get_code({{_, Code, _}, _, _}) ->
    Code.

%% -------------------------------------------------------------------
%% @doc
%% Returns HTTP response body.
%% @end
%% -------------------------------------------------------------------
-spec get_body(HttpcResult :: tuple()) -> Body :: list().

get_body({_, _, Body}) ->
    Body.

%% -------------------------------------------------------------------
%% @doc
%% Catch redirect and GET response from redirect URL
%% > {_, Resp1} = shot_utils:http(get, "http://google.com").
%% > {_, Resp2} = shot_utils:redirect(Resp).
%% @end
%% -------------------------------------------------------------------
-spec redirect(HttpcResult :: tuple()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

redirect({{_, 301, _}, Headers, _}) ->
    http(#{m => get, u => proplists:get_value("location", Headers)}).

%% -------------------------------------------------------------------
%% @doc
%% Catch redirect URL
%% @end
%% -------------------------------------------------------------------
-spec catch_redirect(HttpcResult :: tuple()) -> Result :: tuple() | list().

catch_redirect({{_, 301, _}, Headers, _} = Resp) ->
    proplists:get_value("location", Headers, Resp);
catch_redirect(Resp) ->
    Resp.

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
    binary_to_list(lists:last(binary:split(list_to_binary(Path), <<"/">>, [global]))).

%% -------------------------------------------------------------------
%% @doc
%% HTTPC request with/without headers
%% #{m => get, u => "https://httpbin.org/bearer", h => #{"Authorization" => "Bearer dXNlcjpwYXNz"}}
%% @end
%% -------------------------------------------------------------------
-spec httpc_request(Params :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple()}.

httpc_request(#{m := Method, u := URL, h := Headers, b := Body, ct := ContentType}) ->
    httpc:request(Method, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Headers), ContentType, Body}, [?NO_REDIRECT], []);
httpc_request(#{m := Method, u := URL, b := Body, ct := ContentType}) ->
    httpc:request(Method, {URL, [], ContentType, Body}, [?NO_REDIRECT], []);
httpc_request(#{m := Method, u := URL, h := Headers}) ->
    httpc:request(Method, {URL, maps:fold(fun(K, V, Acc) -> [{K, V} | Acc] end, [], Headers)}, [?NO_REDIRECT], []).
