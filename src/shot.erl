-module(shot).

%%% ==================================================================
%%% API
%%% ==================================================================

-export([
  get/1,
  put/1,
  post/1,
  delete/1,
  multipart/1
]).

%%% ==================================================================
%%% Macros
%%% ==================================================================

-define(BAD_ARG, {error, bad_arg}).

%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% GET request
%% @end
%% -------------------------------------------------------------------
-spec get(Data :: maps:map() | lists:list()) -> {ok, Result :: tuple()} | {error, Reason :: tuple() | bad_arg}.

get(#{u := _} = Data) -> shot_utils:http(maps:merge(#{m => get}, Data));
get(URL) when is_list(URL) -> shot_utils:http(#{m => get, u => URL});
get(_) -> ?BAD_ARG.

%% -------------------------------------------------------------------
%% @doc
%% PUT request
%% @end
%% -------------------------------------------------------------------
-spec put(Data :: maps:map() | lists:list()) -> {ok, Result :: tuple()} | {error, Reason :: tuple() | bad_arg}.

put(#{u := _} = Data) -> shot_utils:http(maps:merge(#{m => put}, Data));
put(URL) when is_list(URL) -> shot_utils:http(#{m => put, u => URL});
put(_) -> ?BAD_ARG.

%% -------------------------------------------------------------------
%% @doc
%% POST request
%% @end
%% -------------------------------------------------------------------
-spec post(Data :: maps:map() | lists:list()) -> {ok, Result :: tuple()} | {error, Reason :: tuple() | bad_arg}.

post(#{u := _} = Data) -> shot_utils:http(maps:merge(#{m => post}, Data));
post(URL) when is_list(URL) -> shot_utils:http(#{m => post, u => URL});
post(_) -> ?BAD_ARG.

%% -------------------------------------------------------------------
%% @doc
%% DELETE request
%% @end
%% -------------------------------------------------------------------
-spec delete(Data :: maps:map() | lists:list()) -> {ok, Result :: tuple()} | {error, Reason :: tuple() | bad_arg}.

delete(#{u := _} = Data) -> shot_utils:http(maps:merge(#{m => delete}, Data));
delete(URL) when is_list(URL) -> shot_utils:http(#{m => delete, u => URL});
delete(_) -> ?BAD_ARG.

%% -------------------------------------------------------------------
%% @doc
%% MULTIPART request
%% @end
%% -------------------------------------------------------------------
-spec multipart(Data :: maps:map()) -> {ok, Result :: tuple()} | {error, Reason :: tuple() | bad_arg}.

multipart(#{} = Data)  -> shot_utils:multipart(Data);
multipart(_) -> ?BAD_ARG.
