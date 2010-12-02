-module(dummy).

-export([init/0, dummy/0]).

init() ->
    spawn_link(dummy,dummy,[]).

dummy() ->
    X = random:uniform(20),
    Y = random:uniform(10),
    frame ! {change_cell, X, Y, purple},
    receive 
        after 100 ->
                dummy:dummy()
        end.
    
