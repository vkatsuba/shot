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
