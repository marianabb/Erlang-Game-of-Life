-module(printer).

-export([init/2, print_loop/0]).

init(W, H) ->
    % Intialize frame
    frame:init(),
    frame ! {set_w, W},
    frame ! {set_h, H},
    frame ! {set_head, "Game of Life"},
    frame ! {set_foot, "Board size: 4x4"}, %TODO: Descablear aqui

    % Create printer
    register(printer, spawn_link(printer,print_loop,[])),
    io:format("Printer is up and running!~n").

% Example: printer ! {self(), {print_cell, 0, 0, 'alive'}}.
print_loop() ->
    receive
        {From, {print_cell, X, Y, Status}} ->
            io:format("----------I am the printer, will print ~p for ~p~p~n", [Status, X, Y]),
            case Status of
                'alive' -> frame ! {change_cell, X, Y, purple};
                'dead' -> frame ! {change_cell, X, Y, white}
            end,
            printer:print_loop();
        {From, {clear}} ->
            io:format("I am the printer, clearing the board~n"),
            frame ! reset_cells,
            printer:print_loop()
    end.

