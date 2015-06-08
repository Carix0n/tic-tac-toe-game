-module(game_http_server).
-author("pdanilov").

-export([who_plays/3, join/3, leave/3, get_field/3, make_turn/3, reset/3, who_won/3]).

-define(LOGIC, {global, logic}).

ct_string(json) -> "Content-type: application/json\r\n\r\n";
ct_string(text) -> "Content-type: text/plain\r\n\r\n".

who_plays(SessionId, _, _) ->
    {Players, Whose_turn} = gen_server:call(?LOGIC, {who_plays}),
    Player_list = lists:map(fun(Player) ->
        io_lib:format("~s", [Player])
    end, Players),
    WhoPlaysJSON = "{\"players\": [" ++ string:join(Player_list, ", ") ++ io_lib:format("],~n\"whose_turn\": ~s", [Whose_turn]) ++ "}",
    mod_esi:deliver(SessionId, ct_string(json) ++ WhoPlaysJSON).

join(SessionId, _, In) ->
    Name = http_uri:decode(In),
    Status = gen_server:call(?LOGIC, {join, Name}),
    mod_esi:deliver(SessionId, ct_string(text) ++ atom_to_list(Status)).

leave(SessionId, _, In) ->
    Name = http_uri:decode(In),
    gen_server:cast(?LOGIC, {leave, Name}),
    mod_esi:deliver(SessionId, ct_string(text) ++ "ok").

get_field(SessionId, _, _) ->
    FieldItems = dict:to_list(gen_server:call(?LOGIC, {get_field})),
    ScreenedItems = lists:map(fun({{X,Y}, Value}) ->
        io_lib:format("{\"x\": ~p, \"y\": ~p, \"player_name\": \"~s\"}", [X, Y, Value])
    end, FieldItems),
    FieldJSON = "[" ++ string:join(ScreenedItems, ", ") ++ "]",
    mod_esi:deliver(SessionId, ct_string(json) ++ FieldJSON).

make_turn(SessionId, _, In) ->
    Request = http_uri:decode(In),
    WordsCount = string:words(Request, 47), %% 47 - is a code of "/"
    case WordsCount of
        3 ->
            Name = string:sub_word(Request, 1, 47),
            {X, _} = string:to_integer(string:sub_word(Request, 2, 47)),
            {Y, _} = string:to_integer(string:sub_word(Request, 3, 47)),
            Cell_status = gen_server:call(?LOGIC, {get_cell, X, Y}),
            case Cell_status of
                {is_not_free, _} -> mod_esi:deliver(SessionId, ct_string(text) ++ "not_empty_cell");
                if_free ->
                    Status = gen_server:call(?LOGIC, {make_turn, Name, X, Y}),
                    mod_esi:deliver(SessionId, ct_string(text) ++ atom_to_list(Status))
            end;
        _ -> mod_esi:deliver(SessionId, ct_string(text) ++ "bad_request")
    end.

reset(_SessionId, _, _) ->
    gen_server:call(?LOGIC, {reset}).
%%    mod_esi:deliver(SessionId, ct_string(text) ++ "ok"). %% Not sure whether deliver is needed or not

who_won(SessionId, _, _) ->
    Status = gen_server:call(?LOGIC, {who_won}),
    mod_esi:deliver(SessionId, ct_string(json) ++ atom_to_list(Status)).