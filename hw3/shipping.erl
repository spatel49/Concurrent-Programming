% I pledge my honor that I have abided by the Stevens Honor System
% Siddhanth Patel and Fabricio Flores

-module(shipping).
-compile(export_all).
-include_lib("./shipping.hrl").

mapl(_F, [])->
    [];
mapl(F, [H | T])->
    [F(H) | mapl(F, T)].

get_ship(Shipping_State, Ship_ID) ->
    A = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if
        A == false ->
            error;
        true ->
            A
    end.

get_container(Shipping_State, Container_ID) ->
    A = lists:keyfind(Container_ID, #container.id, Shipping_State#shipping_state.containers),
    if
        A == false ->
            error;
        true ->
            A
    end.

get_port(Shipping_State, Port_ID) ->
    A = lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports),
    if
        A == false ->
            error;
        true ->
            A
    end.

get_occupied_docks(Shipping_State, Port_ID) ->
    lists:filtermap(fun(X) ->
        case X of
            {Port_ID, Dock, _S} -> {true, Dock};
            {_, _, _} -> false end
        end, Shipping_State#shipping_state.ship_locations).

get_ship_location(Shipping_State, Ship_ID) ->
    A = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if
        A == false -> error;
        true -> lists:nth(1, lists:filtermap(fun(X)->
            case X of 
                {Port_ID, Dock, Ship_ID} -> {true, {Port_ID, Dock}};
                {_, _, _} -> false end
            end, Shipping_State#shipping_state.ship_locations))
    end.

get_container_weight(Shipping_State, Container_IDs) ->
    A = lists:filtermap(fun(X) ->
        case get_container(Shipping_State,X) of
            {_, _, W} -> {true, W};
            _ -> false end end, Container_IDs),
    B = length(A) == length(Container_IDs),
    if
        B == false -> error;
        true -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, A)
    end.

get_ship_weight(Shipping_State, Ship_ID) ->
    case maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory) of
        {ok, CID} -> get_container_weight(Shipping_State, CID);
        error -> error
    end.

load_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Z = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if 
        Z == false -> error;
        true -> A = get_ship_location(Shipping_State, Ship_ID), %Gets ship location {port_id, dock_id}
                C = lists:nth(1, [B || {B,_} <- [A]]), % Gets port_id
                E = element(2, lists:nth(C, maps:to_list(Shipping_State#shipping_state.port_inventory))), %gets all container ids in port_id
                F = element(2, lists:nth(C, maps:to_list(Shipping_State#shipping_state.ship_inventory))), % gets all container ids in port_id of ship inventory
                ShipCapacity = (get_ship(Shipping_State, Ship_ID))#ship.container_cap,
                G = sofs:is_subset(sofs:set(Container_IDs), sofs:set(E)), %checks if container_ids is a subset of all containerids in port_id
                if
                    (G == false) or (length(F) + length(Container_IDs) > ShipCapacity) -> error;
                    true -> J = maps:update(C, lists:append(F, Container_IDs), Shipping_State#shipping_state.ship_inventory),
                            K = maps:update(C, E -- Container_IDs, Shipping_State#shipping_state.port_inventory),
                            Shipping_State#shipping_state{
                                ship_inventory = J,
                                port_inventory = K
                            }
                end
    end.

unload_ship_all(Shipping_State, Ship_ID) ->
    A = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if 
        A == false -> error;
        true -> {ok, Container_IDs} = maps:find(Ship_ID, Shipping_State#shipping_state.ship_inventory),
                unload_ship(Shipping_State, Ship_ID, Container_IDs)
    end.

unload_ship(Shipping_State, Ship_ID, Container_IDs) ->
    Z = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    if
        Z /= false ->
            A = get_ship_location(Shipping_State, Ship_ID), %Gets ship location {port_id, dock_id}
            C = lists:nth(1, [B || {B,_} <- [A]]), % Gets port_id
            E = element(2, lists:nth(C, maps:to_list(Shipping_State#shipping_state.port_inventory))), %gets all container ids in port_id
            F = element(2, lists:nth(Ship_ID, maps:to_list(Shipping_State#shipping_state.ship_inventory))), % gets all container ids in port_id of ship inventory
            ContainerCap = (get_port(Shipping_State, C))#port.container_cap,
            G = sofs:is_subset(sofs:set(Container_IDs), sofs:set(F)), %checks if container_ids is a subset of all containerids in ship inventory
            if
                (G == false) or (length(E) + length(Container_IDs) > ContainerCap) -> io:format("The given containers are not all on the same ship...~n"), error;
                true -> J = maps:update(Ship_ID, F -- Container_IDs, Shipping_State#shipping_state.ship_inventory),
                        K = maps:update(C, lists:append(E, Container_IDs), Shipping_State#shipping_state.port_inventory),
                        Shipping_State#shipping_state{
                            ship_inventory = J,
                            port_inventory = K
                        }
            end;
        true -> error
    end.

set_sail(Shipping_State, Ship_ID, {Port_ID, Dock}) ->
    A = lists:keyfind(Ship_ID, #ship.id, Shipping_State#shipping_state.ships),
    Z = lists:keyfind(Port_ID, #port.id, Shipping_State#shipping_state.ports),
    if
        (A /= false) and (Z /= false)->
            {_One, _Two, _Three, DocksList, _Five} = Z,
            Y = lists:member(Dock, DocksList),
            if 
                Y /= false ->
                    B = get_occupied_docks(Shipping_State, Port_ID),
                    C = sofs:is_subset(sofs:set([Dock]), sofs:set(B)), %checks if dock is occupied, returns true if so
                    E = lists:filtermap(fun(X) ->
                        case X of
                            {_, _, Ship_ID} -> {true, {Port_ID, Dock, Ship_ID}};
                            _ -> {true, X} end end, Shipping_State#shipping_state.ship_locations),
                    if 
                        C -> error;
                        true -> Shipping_State#shipping_state{
                                    ship_locations = E
                                }
                    end;
                true -> error
            end;
        true -> error
    end.
%% Determines whether all of the elements of Sub_List are also elements of Target_List
%% @returns true is all elements of Sub_List are members of Target_List; false otherwise
is_sublist(Target_List, Sub_List) ->
    lists:all(fun (Elem) -> lists:member(Elem, Target_List) end, Sub_List).




%% Prints out the current shipping state in a more friendly format
print_state(Shipping_State) ->
    io:format("--Ships--~n"),
    _ = print_ships(Shipping_State#shipping_state.ships, Shipping_State#shipping_state.ship_locations, Shipping_State#shipping_state.ship_inventory, Shipping_State#shipping_state.ports),
    io:format("--Ports--~n"),
    _ = print_ports(Shipping_State#shipping_state.ports, Shipping_State#shipping_state.port_inventory).


%% helper function for print_ships
get_port_helper([], _Port_ID) -> error;
get_port_helper([ Port = #port{id = Port_ID} | _ ], Port_ID) -> Port;
get_port_helper( [_ | Other_Ports ], Port_ID) -> get_port_helper(Other_Ports, Port_ID).


print_ships(Ships, Locations, Inventory, Ports) ->
    case Ships of
        [] ->
            ok;
        [Ship | Other_Ships] ->
            {Port_ID, Dock_ID, _} = lists:keyfind(Ship#ship.id, 3, Locations),
            Port = get_port_helper(Ports, Port_ID),
            {ok, Ship_Inventory} = maps:find(Ship#ship.id, Inventory),
            io:format("Name: ~s(#~w)    Location: Port ~s, Dock ~s    Inventory: ~w~n", [Ship#ship.name, Ship#ship.id, Port#port.name, Dock_ID, Ship_Inventory]),
            print_ships(Other_Ships, Locations, Inventory, Ports)
    end.

print_containers(Containers) ->
    io:format("~w~n", [Containers]).

print_ports(Ports, Inventory) ->
    case Ports of
        [] ->
            ok;
        [Port | Other_Ports] ->
            {ok, Port_Inventory} = maps:find(Port#port.id, Inventory),
            io:format("Name: ~s(#~w)    Docks: ~w    Inventory: ~w~n", [Port#port.name, Port#port.id, Port#port.docks, Port_Inventory]),
            print_ports(Other_Ports, Inventory)
    end.
%% This functions sets up an initial state for this shipping simulation. You can add, remove, or modidfy any of this content. This is provided to you to save some time.
%% @returns {ok, shipping_state} where shipping_state is a shipping_state record with all the initial content.
shipco() ->
    Ships = [#ship{id=1,name="Santa Maria",container_cap=20},
              #ship{id=2,name="Nina",container_cap=20},
              #ship{id=3,name="Pinta",container_cap=20},
              #ship{id=4,name="SS Minnow",container_cap=20},
              #ship{id=5,name="Sir Leaks-A-Lot",container_cap=20}
             ],
    Containers = [
                  #container{id=1,weight=200},
                  #container{id=2,weight=215},
                  #container{id=3,weight=131},
                  #container{id=4,weight=62},
                  #container{id=5,weight=112},
                  #container{id=6,weight=217},
                  #container{id=7,weight=61},
                  #container{id=8,weight=99},
                  #container{id=9,weight=82},
                  #container{id=10,weight=185},
                  #container{id=11,weight=282},
                  #container{id=12,weight=312},
                  #container{id=13,weight=283},
                  #container{id=14,weight=331},
                  #container{id=15,weight=136},
                  #container{id=16,weight=200},
                  #container{id=17,weight=215},
                  #container{id=18,weight=131},
                  #container{id=19,weight=62},
                  #container{id=20,weight=112},
                  #container{id=21,weight=217},
                  #container{id=22,weight=61},
                  #container{id=23,weight=99},
                  #container{id=24,weight=82},
                  #container{id=25,weight=185},
                  #container{id=26,weight=282},
                  #container{id=27,weight=312},
                  #container{id=28,weight=283},
                  #container{id=29,weight=331},
                  #container{id=30,weight=136}
                 ],
    Ports = [
             #port{
                id=1,
                name="New York",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=2,
                name="San Francisco",
                docks=['A','B','C','D'],
                container_cap=200
               },
             #port{
                id=3,
                name="Miami",
                docks=['A','B','C','D'],
                container_cap=200
               }
            ],
    %% {port, dock, ship}
    Locations = [
                 {1,'B',1},
                 {1, 'A', 3},
                 {3, 'C', 2},
                 {2, 'D', 4},
                 {2, 'B', 5}
                ],
    Ship_Inventory = #{
      1=>[14,15,9,2,6],
      2=>[1,3,4,13],
      3=>[],
      4=>[2,8,11,7],
      5=>[5,10,12]},
    Port_Inventory = #{
      1=>[16,17,18,19,20],
      2=>[21,22,23,24,25],
      3=>[26,27,28,29,30]
     },
    #shipping_state{ships = Ships, containers = Containers, ports = Ports, ship_locations = Locations, ship_inventory = Ship_Inventory, port_inventory = Port_Inventory}.
