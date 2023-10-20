%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2023, 2600Hz
%%% @doc Sends/receives whiteboard state/events to/from whiteboard collaborators
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @author Daniel Finke <danielfinke2011@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(bh_collabazoo).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([init/0]).

%% Blackhole binding callbacks
-export([validate_command/2
        ,handle_command/2
        ,validate_bindings/2
        ,bindings/2
        ]).

-include("collabazoo.hrl").

-define(BINDING(AccountId), <<"collabazoo.event.", AccountId/binary>>).

-type command() :: 'draw' | 'clear'.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the blackhole bindings for this module.
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = blackhole_bindings:bind(<<"blackhole.validate.collabazoo">>, ?MODULE, 'validate_command'),
    _ = blackhole_bindings:bind(<<"blackhole.command.collabazoo">>, ?MODULE, 'handle_command'),
    _ = blackhole_bindings:bind(<<"blackhole.events.validate.collabazoo">>, ?MODULE, 'validate_bindings'),
    _ = blackhole_bindings:bind(<<"blackhole.events.bindings.collabazoo">>, ?MODULE, 'bindings'),
    'ok'.

%%%=============================================================================
%%% Blackhole binding callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Validate a command.
%% @end
%%------------------------------------------------------------------------------
-spec validate_command(bh_context:context(), kz_json:object()) -> bh_context:context().
validate_command(Context, _Payload) ->
    %% TODO
    Context.

%%------------------------------------------------------------------------------
%% @doc Handle a command.
%% @end
%%------------------------------------------------------------------------------
-spec handle_command(bh_context:context(), kz_json:object()) -> bh_context:context().
handle_command(Context, Payload) ->
    Data = kz_json:get_json_value(<<"data">>, Payload),
    handle_command(Context, Data, command(Data)).

%%------------------------------------------------------------------------------
%% @doc Validate a subscription for events bound by this module.
%% @end
%%------------------------------------------------------------------------------
-spec validate_bindings(bh_context:context(), map()) -> bh_context:context().
validate_bindings(Context, #{keys := [<<"event">>]}) -> Context;
validate_bindings(Context, #{keys := Keys}) ->
    bh_context:add_error(Context, <<"invalid format for collabazoo subscription : ", (kz_binary:join(Keys))/binary>>).

%%------------------------------------------------------------------------------
%% @doc Get the bindings for events bound by this module.
%% @end
%%------------------------------------------------------------------------------
-spec bindings(bh_context:context(), map()) -> map().
bindings(_Context, #{account_id := AccountId
                    ,keys := [<<"event">>=Key]
                    }=Map) ->
    Requested = <<"collabazoo.", Key/binary>>,
    Subscribed = [?BINDING(AccountId)],
    Listeners = [{'amqp', 'collabazoo', event_binding_options(AccountId)}],
    Map#{requested => Requested
        ,subscribed => Subscribed
        ,listeners => Listeners
        }.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Handle a specific command.
%% @end
%%------------------------------------------------------------------------------
-spec handle_command(bh_context:context(), kz_json:object(), command()) -> bh_context:context().
handle_command(Context, Data, 'draw') ->
    SessionId = bh_context:websocket_session_id(Context),
    Req = [{<<"Account-ID">>, kz_json:get_ne_binary_value(<<"account_id">>, Data)}
          ,{<<"Collaborator-ID">>, SessionId}
          ,{<<"Points">>, kz_json:get_list_value(<<"points">>, Data)}
          ,{<<"Point-Size">>, kz_json:get_integer_value(<<"point_size">>, Data)}
          ,{<<"RGB">>, kz_json:get_list_value(<<"rgb">>, Data)}
          | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    %% TODO: error handling
    'ok' = kz_amqp_worker:cast(Req, fun kapi_collabazoo:publish_draw/1),
    Context;
handle_command(Context, Data, 'clear') ->
    Req = [{<<"Account-ID">>, kz_json:get_ne_binary_value(<<"account_id">>, Data)}
          | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    %% TODO: error handling
    'ok' = kz_amqp_worker:cast(Req, fun kapi_collabazoo:publish_clear/1),
    Context.

%%------------------------------------------------------------------------------
%% @doc Get the command from a payload.
%% @end
%%------------------------------------------------------------------------------
-spec command(kz_json:object()) -> command().
command(JObj) ->
    case kz_json:get_ne_binary_value(<<"command">>, JObj) of
        <<"draw">> -> 'draw';
        <<"clear">> -> 'clear'
    end.

%%------------------------------------------------------------------------------
%% @doc Get the options for event bindings.
%% @end
%%------------------------------------------------------------------------------
-spec event_binding_options(kz_term:ne_binary()) -> kz_term:proplist().
event_binding_options(AccountId) ->
    %% TODO: support federation?
    [{'account_id', AccountId}].
