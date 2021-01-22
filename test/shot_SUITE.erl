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

-export([test_travis/1]).

%%% ==================================================================
%%% Includes
%%% ==================================================================
-include_lib("common_test/include/ct.hrl").
-include("shot.hrl").

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
    {group, shot_delete},
    {group, travis}
  ].

%% -------------------------------------------------------------------
%% @doc
%% Groups
%% @end
%% -------------------------------------------------------------------
-spec groups() -> lists:list().

groups() ->
ct_helper:repeat_all_until_all_ok([
{travis, [sequence], [
  test_travis
]},
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
]).

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

test_travis(_) ->
    case persistent_term:get(tester, false) of 
    	false ->
    		persistent_term:put(tester, true),
    		ct:fail("fail");
    	_ ->
            ct:comment("~p", ["comment"])
    end.

%% -------------------------------------------------------------------
%% @doc
%% PUT request without headers
%% @end
%% -------------------------------------------------------------------
-spec shot_put_without_headers_must_ok(config()) -> ok.

shot_put_without_headers_must_ok(_Config) ->
  {ok, Url} = application:get_env(urls, put),
  case shot:put(Url) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_put_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
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
  {ok, Url} = application:get_env(urls, get),
  case shot:get(Url) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_get_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
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
  {ok, Url} = application:get_env(urls, bearer),
  Data = #{
    u => Url,
    h => #{"Authorization" => "Bearer dXNlcjpwYXNz"}
  },
  case shot:get(Data) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_get_with_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
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
  {ok, Url} = application:get_env(urls, post),
  case shot:post(Url) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_post_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
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
  {ok, Url} = application:get_env(urls, anything),
  Data = #{
    u => Url,
    b => "{\"foo\":[\"bing\",2.3,true]}",
    ct => "application/json",
    h => #{"Authorization" => "Basic dmthdHN1YmE6JDFxMnczZTQk"}
  },
  case shot:post(Data) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_post_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
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
  {ok, Url} = application:get_env(urls, delete),
  case shot:delete(Url) of
    {ok, HttpcResult} ->
      case shot_utils:get_code(HttpcResult) of
        200 -> ct:comment("HttpcResult = ~p", [HttpcResult]);
        _ -> ct:fail("shot_delete_without_headers_must_ok/1 failed (HTTP response code MUST be equal to 200): ~p", [HttpcResult])
      end;
    {error, Reason} ->
      ct:fail(Reason)
  end.
