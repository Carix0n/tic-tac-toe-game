-module(additional_functions).
-author("pdanilov").

-export([first_entry/2, direction_sum/6]).

first_entry(Value, List) when is_list(List) ->
    first_entry(Value, List, 1).

first_entry(_Value, List, _Depth) when List == [] -> 0;
first_entry(Value, List, Depth) ->
    [Head|Tail] = List,
    case Head == Value of
        true -> Depth;
        false -> first_entry(Value, Tail, Depth + 1)
    end.

direction_sum(Dict, X, Y, AddX, AddY, EndLevel) ->
    direction_sum(Dict, X + AddX, Y + AddY, AddX, AddY, 1, EndLevel, 0).

direction_sum(Dict, X, Y, AddX, AddY, Level, EndLevel, Count) ->
    Belongs = dict:is_key({X,Y}, Dict),
    if
        Belongs and (Level =< EndLevel) ->
            direction_sum(Dict, X + AddX, Y + AddY, AddX, AddY, Level + 1, EndLevel, Count + 1);
        true ->
            Count
    end.