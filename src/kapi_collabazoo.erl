%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2023, 2600Hz
%%% @doc KAPI for sending/receiving whiteboard state/events
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @author Daniel Finke <danielfinke2011@gmail.com>
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_collabazoo).

%%%=============================================================================
%%% Export and Defs
%%%=============================================================================

%% External API
-export([api_definitions/0, api_definition/1]).
-export([draw/1, draw_v/1
        ,publish_draw/1, publish_draw/2
        ,clear/1, clear_v/1
        ,publish_clear/1, publish_clear/2
        ]).
-export([bind_q/2, unbind_q/2
        ,declare_exchanges/0
        ]).

-include_lib("kazoo_amqp/include/kz_api.hrl").
-include("collabazoo.hrl").

-define(EVENT_CATEGORY, <<"collabazoo">>).
-define(EVENT_TYPE_HEADERS(EventName), [{<<"Event-Category">>, ?EVENT_CATEGORY}
                                       ,{<<"Event-Name">>, EventName}
                                       ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [draw_definition()
    ,clear_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"draw">>) -> draw_definition();
api_definition(<<"clear">>) -> clear_definition().

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec draw_definition() -> kapi_definition:api().
draw_definition() ->
    EventName = <<"draw">>,
    #kapi_definition{name = EventName
                    ,friendly_name = <<"Draw">>
                    ,description = <<"Draw some pixels to whiteboard">>
                    ,build_fun = fun draw/1
                    ,validate_fun = fun draw_v/1
                    ,publish_fun = fun publish_draw/2
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Collaborator-ID">>
                                        ,<<"Points">>
                                        ,<<"Point-Size">>
                                        ,<<"RGB">>
                                        ]
                    ,optional_headers = []
                    ,values = ?EVENT_TYPE_HEADERS(EventName)
                     %% TODO
                    ,types = []
                    }.

-spec clear_definition() -> kapi_definition:api().
clear_definition() ->
    EventName = <<"clear">>,
    #kapi_definition{name = EventName
                    ,friendly_name = <<"Clear">>
                    ,description = <<"Clear whiteboard and start fresh">>
                    ,build_fun = fun clear/1
                    ,validate_fun = fun clear_v/1
                    ,publish_fun = fun publish_clear/2
                    ,required_headers = [<<"Account-ID">>]
                    ,optional_headers = []
                    ,values = ?EVENT_TYPE_HEADERS(EventName)
                     %% TODO
                    ,types = []
                    }.

-spec bind_q(kz_term:api_ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    kz_amqp_util:bind_q_to_kapps(Queue, event_routing_key(AccountId)).

-spec unbind_q(kz_term:api_ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    AccountId = props:get_value('account_id', Props, <<"*">>),
    _ = kz_amqp_util:unbind_q_from_kapps(Queue, event_routing_key(AccountId)),
    'ok'.

-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:kapps_exchange().

%%------------------------------------------------------------------------------
%% @doc Generic function to build API payload.
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), kapi_definition:api()) -> api_formatter_return().
build_message(Prop, #kapi_definition{required_headers = ReqH
                                    ,optional_headers = OptH
                                    ,validate_fun = Validate
                                    ,name = _Name
                                    }) when is_list(Prop) ->
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(_Name)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, #kapi_definition{required_headers = ReqH
                               ,values = Values
                               ,types = Types
                               }) when is_list(Prop) ->
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Draw some pixels to whiteboard.
%% @end
%%------------------------------------------------------------------------------
-spec draw(kz_term:api_terms()) -> api_formatter_return().
draw(Req) ->
    build_message(Req, draw_definition()).

-spec draw_v(kz_term:api_terms()) -> boolean().
draw_v(Req) ->
    validate(Req, draw_definition()).

-spec publish_draw(kz_term:api_terms()) -> 'ok'.
publish_draw(Req) ->
    publish_draw(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_draw(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_draw(Req, ContentType) ->
    AccountId = kz_api:account_id(Req),
    #kapi_definition{build_fun = BuildFun
                    ,values = Values
                    } = draw_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, Values, BuildFun),
    RoutingKey = event_routing_key(AccountId),
    kz_amqp_util:kapps_publish(RoutingKey, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Clear whiteboard and start fresh.
%% @end
%%------------------------------------------------------------------------------
-spec clear(kz_term:api_terms()) -> api_formatter_return().
clear(Req) ->
    build_message(Req, clear_definition()).

-spec clear_v(kz_term:api_terms()) -> boolean().
clear_v(Req) ->
    validate(Req, clear_definition()).

-spec publish_clear(kz_term:api_terms()) -> 'ok'.
publish_clear(Req) ->
    publish_clear(Req, ?DEFAULT_CONTENT_TYPE).

-spec publish_clear(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_clear(Req, ContentType) ->
    AccountId = kz_api:account_id(Req),
    #kapi_definition{build_fun = BuildFun
                    ,values = Values
                    } = clear_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(Req, Values, BuildFun),
    RoutingKey = event_routing_key(AccountId),
    kz_amqp_util:kapps_publish(RoutingKey, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Get the routing key for events.
%% @end
%%------------------------------------------------------------------------------
-spec event_routing_key(kz_term:ne_binary()) -> kz_term:ne_binary().
event_routing_key(AccountId) ->
    <<"collabazoo.event.", AccountId/binary>>.
