
%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Eric Merrit <ericbmerritt@gmail.com>
%%% @copyright (C) Erlware, LLC.
%%% @doc
%%%  This provides a trivial way to integrate the relx release builder into
%%%  rebar.
%%%
%%%  It has two optional configuration values. Those are
%%%  1) relx_libdirs
%%%  2) relx_output
%%%
%%%  These have sane defaults but can be specified by the user.
-module(rebar_relx_plugin).

-export([release/2, tar/2]).

%%============================================================================
%% API
%%============================================================================
-spec release/2 :: (term(), file:path()) -> ok | no_return().
release(Config, _AppFile) ->
    case new_enough_rebar() of
        true ->
            run_on_base_dir(release, Config);
        false ->
            rebar_utils:abort("Rebar version is to old to run the relx plugin", [])
    end.

-spec tar/2 :: (term(), file:path()) -> ok | no_return().
tar(Config, _AppFile) ->
    case new_enough_rebar() of
        true ->
            run_on_base_dir(tar, Config);
        false ->
            rebar_utils:abort("Rebar version is too old to run the relx plugin", [])
    end.

%%============================================================================
%% Internal Functions
%%============================================================================
-spec new_enough_rebar() -> boolean().
new_enough_rebar() ->
    Exports = rebar_utils:module_info(exports),
    lists:member({processing_base_dir, 1}, Exports).

-spec run_on_base_dir(release | tar, term()) -> ok | no_return().
run_on_base_dir(Command, Config) ->
    case rebar_utils:processing_base_dir(Config) of
        true ->
            check_for_relx_config(Command, Config);
        false ->
            ok
   end.

-spec check_for_relx_config(release | tar, term()) -> ok | no_return().
check_for_relx_config(Command, Config) ->
    CurDir = filename:absname(rebar_utils:get_cwd()),
    RelxFile = filename:join(CurDir, "relx.config"),
    case filelib:is_regular(RelxFile) of
        true ->
            do(Command, Config, RelxFile);
        false ->
            ok
    end.

-spec get_command(release | tar) -> string().
get_command(release) -> "release";
get_command(tar) -> "tar".

-spec do(release | tar, term(), file:path()) -> ok | no_return().
do(Command, Config, RelxFile) ->
    LibDirs = rebar_config:get_list(Config, relx_libdirs, []),
    LogLevel = get_log_level(Config),
    OutputDir = rebar_config:get(Config, relx_output, "rel"),
    case relx:do([{lib_dirs, LibDirs},
                  {log_level, LogLevel},
                  {output_dir, OutputDir},
                  {config, RelxFile}],
                  [get_command(Command)]) of
        {ok, _} ->
            ok;
        Error = {error, _} ->
            rebar_utils:abort("~s", [relx:format_error(Error)])
    end.

-spec get_log_level(term()) -> non_neg_integer().
get_log_level(Config) ->
    Verbosity = rebar_config:get_global(Config, verbose, rebar_log:default_level()),
    case Verbosity of
        error -> 0;
        warn -> 1;
        info -> 2;
        debug -> 3;
        I when erlang:is_integer(I) ->
            I
    end.
