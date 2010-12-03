-module(printer).

-export([init/2, print_loop/3, generate_names/5]).

init(W, H) ->
    % Initialize frame
    frame:init(),
    frame ! {set_w, W},
    frame ! {set_h, H},
    frame ! {set_head, "Game of Life"},
    frame ! {set_foot, "Board size: "++integer_to_list(W)++"x"++integer_to_list(H)},
    
    % Generate the names of the cells based on the size of the board
    Names = generate_names(W, H, 0, 0, []),
    
    % Create printer
    register(printer, spawn_link(printer,print_loop,[Names, W*H, 0])),
    io:format("Printer is up and running!~n").


% Generates the names of all the cells in the board
% using Width and Height
% Example: printer:generate_names(3, 3, 0, 0, []).
generate_names(W, H, Col, Row, Names) 
  when (Col == W) and (Row == H-1) -> Names;

generate_names(W, H, Col, Row, Names) 
  when (Col == W) -> generate_names(W, H, 0, Row+1, Names);

generate_names(W, H, Col, Row, Names) ->
    Name = list_to_atom(integer_to_list(Col) ++ integer_to_list(Row)),
    generate_names(W, H, Col+1, Row, [Name | Names]).
    
% Controls the actions of the printer:
% 1. Check if it's time for the next tick. If it is,
%    send to all cells a message to continue.
% 2. Wait for messages to print cells or clear the board.
% Example: printer ! {self(), {print_cell, 0, 0, 'alive'}}.
print_loop(Names, P_total, P_count) ->
    % If I printed all status of this tick, tell the cells to continue
    if
        (P_total == P_count) -> 
            timer:sleep(1000), %Just in case I'm not done printing
            [Name ! continue || Name <- Names],
            print_loop(Names, P_total, 0);
        true ->
            ok
    end,
    
    receive
        {From, {print_cell, X, Y, Status}} ->
            %io:format("I am the printer, will print ~p for ~p~p~n", [Status, X, Y]),
            case Status of
                'alive' -> frame ! {change_cell, X, Y, purple};
                'dead' -> frame ! {change_cell, X, Y, white}
            end,
            printer:print_loop(Names, P_total, P_count+1);
        {From, {clear}} ->
            io:format("I am the printer, clearing the board~n"),
            frame ! reset_cells,
            printer:print_loop(Names, P_total, 0)
    end.



%[ Pid ! Msg || Pid<-Pids ].
%[ spawn(fun() -> [Pid ! Msg || Pid<-PidsSublist] end) || PidsSublist <- partition(Pids, N) ].
 
