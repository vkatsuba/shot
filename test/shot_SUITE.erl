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

-module(shot_SUITE).

%%% ==================================================================
%%% Common Tests Callbacks Exports
%%% ==================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1
]).

%%% ==================================================================
%%% PUT Exports
%%% ==================================================================

-export([
    shot_put_without_headers_must_ok/1
]).

%%% ==================================================================
%%% GET Exports
%%% ==================================================================

-export([
    shot_get_without_headers_must_ok/1,
    shot_get_with_headers_must_ok/1
]).

%%% ==================================================================
%%% POST Exports
%%% ==================================================================

-export([
    shot_post_without_headers_must_ok/1,
    shot_post_with_headers_must_ok/1
]).

%%% ==================================================================
%%% DELETE Exports
%%% ==================================================================

-export([
    shot_delete_without_headers_must_ok/1
]).

%%% ==================================================================
%%% Specification
%%% ==================================================================

-type config() :: [{atom(), term()}].

%%% ==================================================================
%%% Common Tests Callbacks
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Init all groups
%% @end
%% -------------------------------------------------------------------
-spec all() -> lists:list().

all() ->
    [
        {group, shot_put},
        {group, shot_get},
        {group, shot_post},
        {group, shot_delete}
    ].

%% -------------------------------------------------------------------
%% @doc
%% Groups
%% @end
%% -------------------------------------------------------------------
-spec groups() -> lists:list().

groups() ->
    [
        {shot_put, [sequence], [
            shot_put_without_headers_must_ok
        ]},
        {shot_get, [sequence], [
            shot_get_without_headers_must_ok,
            shot_get_with_headers_must_ok
        ]},
        {shot_post, [sequence], [
            shot_post_without_headers_must_ok,
            shot_post_with_headers_must_ok
        ]},
        {shot_delete, [sequence], [
            shot_delete_without_headers_must_ok
        ]}
    ].

%% -------------------------------------------------------------------
%% @doc
%% Init per suite
%% @end
%% -------------------------------------------------------------------
-spec init_per_suite(config()) -> config().

init_per_suite(Config) ->
    Config.

%% -------------------------------------------------------------------
%% @doc
%% End per suite
%% @end
%% -------------------------------------------------------------------
-spec end_per_suite(config()) -> config().

end_per_suite(Config) ->
    Config.

%%% ==================================================================
%%% Test cases for PUT request with/without headers (shot:put/1)
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% PUT request without headers
%% @end
%% -------------------------------------------------------------------
-spec shot_put_without_headers_must_ok(config()) -> ok.

shot_put_without_headers_must_ok(_Config) ->
    case shot:put("http://httpbin.org/put") of
        {ok, HttpcResult} ->
            case shot_utils:get_code(HttpcResult) of
                200 ->
                    ct:comment("HttpcResult = ~p", [HttpcResult]);
                _ ->
                    ct:fail("shot_put_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
            end;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%%% ==================================================================
%%% Test cases for GET request with/without headers (shot:get/1)
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% GET request without headers
%% @end
%% -------------------------------------------------------------------
-spec shot_get_without_headers_must_ok(config()) -> ok.

shot_get_without_headers_must_ok(_Config) ->
    case shot:get("http://httpbin.org/get") of
        {ok, HttpcResult} ->
            case shot_utils:get_code(HttpcResult) of
                200 ->
                    ct:comment("HttpcResult = ~p", [HttpcResult]);
                _ ->
                    ct:fail("shot_get_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
            end;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% -------------------------------------------------------------------
%% @doc
%% GET request with headers
%% @end
%% -------------------------------------------------------------------
-spec shot_get_with_headers_must_ok(config()) -> ok.

shot_get_with_headers_must_ok(_Config) ->
    Data = #{
        u => "https://httpbin.org/bearer",
        h => #{"Authorization" => "Bearer dXNlcjpwYXNz"}
    },
    case shot:get(Data) of
        {ok, HttpcResult} ->
            case shot_utils:get_code(HttpcResult) of
                200 ->
                    ct:comment("HttpcResult = ~p", [HttpcResult]);
                _ ->
                    ct:fail("shot_get_with_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
            end;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%%% ==================================================================
%%% Test cases for POST request with/without headers (shot:post/1)
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% POST request without headers
%% @end
%% -------------------------------------------------------------------
-spec shot_post_without_headers_must_ok(config()) -> ok.

shot_post_without_headers_must_ok(_Config) ->
    case shot:post("http://httpbin.org/post") of
        {ok, HttpcResult} ->
            case shot_utils:get_code(HttpcResult) of
                200 ->
                    ct:comment("HttpcResult = ~p", [HttpcResult]);
                _ ->
                    ct:fail("shot_post_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
            end;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%% -------------------------------------------------------------------
%% @doc
%% POST request with headers
%% @end
%% -------------------------------------------------------------------
-spec shot_post_with_headers_must_ok(config()) -> ok.

shot_post_with_headers_must_ok(_Config) ->
    Data = #{
        u => "https://httpbin.org/anything",
        b => "{\"foo\":[\"bing\",2.3,true]}",
        ct => "application/json",
        h => #{"Authorization" => "Basic dmthdHN1YmE6JDFxMnczZTQk"}
    },
    case shot:post(Data) of
        {ok, HttpcResult} ->
        case shot_utils:get_code(HttpcResult) of
            200 ->
                ct:comment("HttpcResult = ~p", [HttpcResult]);
            _ ->
                ct:fail("shot_post_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
        end;
        {error, Reason} ->
            ct:fail(Reason)
    end.

%%% ==================================================================
%%% Test cases for DELETE request with/without headers (shot:delete/1)
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% POST request without headers
%% @end
%% -------------------------------------------------------------------
-spec shot_delete_without_headers_must_ok(config()) -> ok.

shot_delete_without_headers_must_ok(_Config) ->
    case shot:delete("http://httpbin.org/delete") of
        {ok, HttpcResult} ->
            case shot_utils:get_code(HttpcResult) of
                200 ->
                    ct:comment("HttpcResult = ~p", [HttpcResult]);
                _ ->
                    ct:fail("shot_delete_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
            end;
        {error, Reason} ->
            ct:fail(Reason)
    end.
