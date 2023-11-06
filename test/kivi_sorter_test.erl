%%%-------------------------------------------------------------------
%% @author: Mateusz Babski
%% @last_updated: 06.11.2023
%%
%% @doc kivi simple key-value database - date sorter test
%% @end
%%%-------------------------------------------------------------------
%%%

-module(kivi_sorter_test).

-include_lib("eunit/include/eunit.hrl").

test_map() ->
    #{
    "GirlCar" =>
        {data,"663276556177704756","Audi",{1699,293885,342329}},
    "FatherCar" =>
        {data,"530193673845343438","BMW",{1699,293910,928505}},
    "SisterCar" =>
        {data,"835430411991216592","Skoda",{1699,293917,472991}},
    "MyCar" =>
        {data,"955639484694277063","Mercedes",{1699,293924,222175}}
    }.

sort_by_id_test() ->
    ?assertEqual(
    [
        {"FatherCar",
        {data,"530193673845343438","BMW",{1699,293910,928505}}},
        {"GirlCar",
        {data,"663276556177704756","Audi",{1699,293885,342329}}},
        {"SisterCar",
        {data,"835430411991216592","Skoda",{1699,293917,472991}}},
        {"MyCar",
        {data,"955639484694277063","Mercedes",{1699,293924,222175}}}
    ], 
    kivi_sorter:sort_data(test_map(), "id")).

sort_by_key_test() ->
    ?assertEqual(
    [
        {"FatherCar",
        {data,"530193673845343438","BMW",{1699,293910,928505}}},
        {"GirlCar",
        {data,"663276556177704756","Audi",{1699,293885,342329}}},
        {"MyCar",
        {data,"955639484694277063","Mercedes",{1699,293924,222175}}},
        {"SisterCar",
        {data,"835430411991216592","Skoda",{1699,293917,472991}}}
    ], 
    kivi_sorter:sort_data(test_map(), "key")).

sort_by_value_test() ->
    ?assertEqual(
    [
        {"GirlCar",
        {data,"663276556177704756","Audi",{1699,293885,342329}}},
        {"FatherCar",
        {data,"530193673845343438","BMW",{1699,293910,928505}}},
        {"MyCar",
        {data,"955639484694277063","Mercedes",{1699,293924,222175}}},
        {"SisterCar",
        {data,"835430411991216592","Skoda",{1699,293917,472991}}}
    ], 
    kivi_sorter:sort_data(test_map(), "value")).

sort_by_updated_test() ->
    ?assertEqual(
    [
        {"GirlCar",
        {data,"663276556177704756","Audi",{1699,293885,342329}}},
        {"FatherCar",
        {data,"530193673845343438","BMW",{1699,293910,928505}}},
        {"SisterCar",
        {data,"835430411991216592","Skoda",{1699,293917,472991}}},
        {"MyCar",
        {data,"955639484694277063","Mercedes",{1699,293924,222175}}}
    ], 
    kivi_sorter:sort_data(test_map(), "updated")).