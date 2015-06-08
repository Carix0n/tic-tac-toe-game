-module(game_logic).
-author("pdanilov").

-behaviour(gen_server).

-export([start/0,
    start_link/0]).

-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(game_state, {field = dict:new(), players = [], whose_turn = 0, who_won = "" }).

start() -> start_link().

start_link() -> gen_server:start_link({global, logic}, ?SERVER, [], []).

init([]) -> {ok, start_game()}.

start_game() -> #game_state{}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call( {who_plays} , _, State) -> {reply, who_plays(State), State};
handle_call( {who_won} , _, State) -> {reply, who_won(State), State};
handle_call( {get_cell, X, Y}, _, State) -> {reply, get_cell(X, Y, State), State};
handle_call( {get_field}, _, State) -> {reply, get_field(State), State};
handle_call( {make_turn, PlayerName, X, Y}, _, State) ->
    {Status, NewState} = try_make_turn(X, Y, PlayerName, State),
    {reply, Status, NewState};
handle_call({join, Name}, _, State) ->
    {Status, NewState} = join_game(Name, State),
    {reply, Status, NewState}.

handle_cast({reset}, _ ) -> {noreply, #game_state{}};
handle_cast({leave, Name}, State) -> {noreply, leave_game(Name, State)};
handle_cast(_Request, State) -> {noreply, State}.

who_plays(State) ->
    Players = State#game_state.players,
    Whose_turn =
        case State#game_state.whose_turn == 0 of
            true -> "";
            false -> lists:nth(State#game_state.whose_turn, State#game_state.players)
        end,
    {Players, Whose_turn}.

who_won(State) ->
    case State#game_state.who_won == [] of
        true -> null;
        false -> lists:nth(State#game_state.who_won, State#game_state.players)
    end.

get_cell(X, Y, State) ->
    case dict:find({X,Y}, State#game_state.field) of
        {ok, Value} -> {is_not_free, Value};
        error -> is_free
    end.

get_field(State) ->
    State#game_state.field.

try_make_turn(X, Y, PlayerName, State) ->
    Whose_turn = lists:nth(State#game_state.whose_turn, State#game_state.players),
    Status =
        if
            ((State#game_state.who_won /= []) and (State#game_state.who_won == PlayerName)) -> game_over_you_win;
            ((State#game_state.who_won /= []) and (State#game_state.who_won /= PlayerName)) -> game_over;
            Whose_turn /= PlayerName -> not_your_turn;
            true -> ok
        end,
    NewState =
        case Status of
            ok ->
                New_field = dict:store({X,Y}, PlayerName, State#game_state.field),
                New_whose_turn =
                    if
                        State#game_state.whose_turn == length(State#game_state.players) -> 1;
                        true -> State#game_state.whose_turn + 1
                    end,
                N = additional_functions:direction_sum(New_field, X, Y, 0, 1, 4),
                NE = additional_functions:direction_sum(New_field, X, Y, 1, 1, 4),
                E = additional_functions:direction_sum(New_field, X, Y, 1, 0, 4),
                SE = additional_functions:direction_sum(New_field, X, Y, 1, -1, 4),
                S = additional_functions:direction_sum(New_field, X, Y, 0, -1, 4),
                SW = additional_functions:direction_sum(New_field, X, Y, -1, -1, 4),
                W = additional_functions:direction_sum(New_field, X, Y, -1, 0, 4),
                NW = additional_functions:direction_sum(New_field, X, Y, -1, 1, 4),
                New_who_won =
                    case (N + S >= 4) or (E + W >= 4) or (NE + SW >= 4) or (NW + SE >= 4) of
                        true -> PlayerName;
                        false -> State#game_state.who_won
                    end,
                State#game_state{field = New_field, whose_turn = New_whose_turn, who_won = New_who_won};
            _ ->
                State
        end,
    {Status, NewState}.

join_game(Name, State) ->
    Status =
        case lists:member(Name, State#game_state.players) of
            true -> player_exists;
            false -> ok
        end,
    NewState =
        case Status of
            player_exists -> State;
            ok -> New_whose_turn =
                case State#game_state.whose_turn == 0 of
                    true -> 1;
                    false -> State#game_state.whose_turn
                end,
                State#game_state{players = lists:append(State#game_state.players, [Name]), whose_turn = New_whose_turn}
        end,
    {Status, NewState}.

leave_game(Name, State) ->
    Length = length(State#game_state.players),
    Player_index = additional_functions:first_entry(Name, State#game_state.players),
    New_whose_turn =
        if
            ((Player_index == State#game_state.whose_turn) and (Player_index == Length)) ->
                case Player_index of
                    1 -> 0;
                    _ -> 1
                end;
            Player_index < State#game_state.whose_turn -> State#game_state.whose_turn - 1;
            true -> State#game_state.whose_turn
        end,
    New_list = lists:delete(Name, State#game_state.players),
    State#game_state{players = New_list, whose_turn = New_whose_turn}.