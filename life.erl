-module(life).
-export([create/0, init_life/5, new_cell/5, 
         cell_loop/7, calculate_future/2, communicate/7,
         calculate_max_n/4, init/0]).


-record(cell, {x, y, now_state}).

% Just a Cell creation test
create() ->    
    X = #cell{x=1, y=2, now_state=alive},
    io:format("x is: ~p", [X]).

init() ->
    %init_life(3, 3, [" X ", " X ", " X "], 0, 0).
    %init_life(4, 4, ["    ", " XX ", " XX ", "    "], 0, 0).
    %init_life(4, 4, ["    ", " XXX", "XXX ", "    "], 0, 0).
    %init_life(6, 6, [" X    ", "  X   ", "XXX   ", "      ", "      ", "      "], 0, 0).
    init_life(26, 10, [" X                        ", 
                       "  X                       ", 
                       "XXX                       ", 
                       "                          ", 
                       "                          ", 
                       "                          ", 
                       "                          ", 
                       "                          ", 
                       "                          ", 
                       "                          "], 0, 0).


% Function that receives the initial state and spawns a 
% process for every square. Every process must have a cell
% with coordinates and state.
% The last argument is a list of strings in which every string
% represents one row of the board.
% Example board: [" X ", " X ", " X "]
% Example call: life:init_life(3, 3, [" X ", " X ", " X "], 0, 0).
% TODO: Verify that Width and Height are correct
init_life(Width, Height, [], _, _) ->
    printer:init(Width, Height),
    ok;
init_life(Width, Height, [Row | Board], N_row, _) when (Row == []) -> 
    init_life(Width, Height, Board, N_row + 1, 0);
init_life(Width, Height, [ [X|XS] | Board], N_row, N_col) ->
    S_row = integer_to_list(N_row),
    S_col = integer_to_list(N_col),
    
    if
        X == 88 -> State = 'alive'; % 88 is ISO for X
        true -> State = 'dead'
    end,
    Pid = spawn(fun () -> new_cell(Width, Height, N_col, N_row, State) end),
    %io:format("Registering PID ~p as ~p~n", [Pid, list_to_atom(S_col ++ S_row)]),
    register(list_to_atom(S_col ++ S_row), Pid),
    init_life(Width, Height, [XS | Board], N_row, N_col + 1).


% Creates a new Cell with the first state and makes the
% process execute cell_loop.
new_cell(W, H, X, Y, State) -> 
    Cell = #cell{x = X, y = Y, now_state = State},

    % Calculate Max_neigh according to the coordinates
    Max_n = calculate_max_n(X, Y, W, H),

    io:format("New cell ~p created with Max_neigh = ~p~n", [Cell, Max_n]),
    % Wait a little before starting the loop
    timer:sleep(1000),
    cell_loop(W, H, Cell, 0, Max_n, 0, 0).


% Calculates the Maximum number of neighbours for a cell.
% Considers 3 cases: center, corner and border.
calculate_max_n(X, Y, W, H) ->
    if 
        (X =/= 0) and (Y =/= 0) and (X =/= W-1) and (Y =/= H-1) ->
            8;
        ((X == 0) or (X+1 == W)) and ((Y == 0) or (Y+1 == H)) ->
            3;
        (((X == 0) or (X+1 == W)) and ((Y > 0) and (Y < H))) or
        (((Y == 0) or (Y+1 == H)) and ((X > 0) and (X < W))) ->
            5
    end.


% The function that controls the actions that every cell
% must accomplish:
% 1. Send my status to all my neighbours
% 2. Check if I have all my neighbours status. 
%    If I do, calculate my next status, print it and change it.
% 3. Wait to receive status from my neighbours.
cell_loop(W, H, Cell, Num_neigh, Max_neigh, Alive_count, Tick) ->

    if
        (Tick == 35) -> %TODO Create Max_ticks or eliminate
            %io:format("NO MORE TICKS!!~n", []),
            self() ! suicide_please;
        true -> ok
    end,

    % Create a process to send my status to all my neighbours in the beginning 
    % of every tick.
    % We know that at the start of a tick Num_neigh == 0
    if (Num_neigh == 0) ->
            spawn(fun () -> sender(Cell#cell.x, Cell#cell.y, Cell#cell.now_state, 0, W, H, Tick) end);
            %communicate(Cell#cell.x, Cell#cell.y, Cell#cell.now_state, 0, W, H);
       true -> ok
    end,
    
    % Check if I have all my neighbors status. If I do, calculate my next status.
    if (Num_neigh == Max_neigh) ->
            %io:format("I am cell ~p and I will calculate my next status~n", [Cell]),
            Future = calculate_future(Alive_count, Cell#cell.now_state),

            % Send my status in the current tick to the printer
            printer ! {self(), {print_cell, Cell#cell.x, Cell#cell.y, Future, Tick}},
       
            %TODO: Maybe wait a little here?
            cell_loop(W, H, {cell, Cell#cell.x, Cell#cell.y, Future}, 0, Max_neigh, 0, Tick+1);

%%             receive
%%                 continue ->
%%                     io:format("Cell ~p was called to continue~n", [Cell]),
%%                     cell_loop(W, H, {cell, Cell#cell.x, Cell#cell.y, Future}, 0, Max_neigh, 0, Tick+1)
%%             end;
       true -> ok
    end,
    
    %io:format("Cell ~p waiting on Tick ~p~n", [Cell, Tick]),
    receive 
        %TODO: Coordinates only for debug. Not using Neighb
        % Only process the message if N_tick corresponds with my Tick
        {Neighb, {st_sent, XN, YN, N_status, N_tick}} when (N_tick == Tick) -> 
            %io:format("I, ~p~p, received status ~p from neighbour ~p~p~n", [Cell#cell.x, Cell#cell.y, N_status, XN, YN]),
         
            case N_status of
                'alive' -> cell_loop(W, H, Cell, Num_neigh+1, Max_neigh, Alive_count+1, Tick);
                'dead' -> cell_loop(W, H, Cell, Num_neigh+1, Max_neigh, Alive_count, Tick)
            end;
        suicide_please ->
            %TODO: unregister myself!
            void
    end.

% Process that sends the status in the current tick to
% all the neighbours of the cell.
% After all the messages have been sent the process dies.
sender(X, Y, Status, Neighbour, W, H, Tick) ->  
    communicate(X, Y, Status, Neighbour, W, H, Tick),
    self() ! die,
    receive 
        die -> void
    end.


% Sends the status of Cell to all its neighbours 
% If the neighbour is outside the board it will not be considered 
communicate(X, Y, _, 8, _, _, _) ->
    %io:format("Cell ~p~p sent state to all its neighbours~n", [X, Y]),
    ok;
communicate(0, Y, Status, Neighbour, W, H, Tick)
  when (Neighbour == 0) or (Neighbour == 3) or (Neighbour == 5) ->
    communicate(0, Y, Status, Neighbour+1, W, H, Tick);

communicate(X, 0, Status, Neighbour, W, H, Tick)
  when (Neighbour == 5) or (Neighbour == 6) or (Neighbour == 7) ->
    communicate(X, 0, Status, Neighbour+1, W, H, Tick);

communicate(X, Y, Status, Neighbour, W, H, Tick)
  when (X+1 == W) and ((Neighbour == 2) or (Neighbour == 4) or (Neighbour == 7)) ->
    communicate(X, Y, Status, Neighbour+1, W, H, Tick);

communicate(X, Y, Status, Neighbour, W, H, Tick)
  when (Y+1 == H) and ((Neighbour == 0) or (Neighbour == 1) or (Neighbour == 2)) ->
    communicate(X, Y, Status, Neighbour+1, W, H, Tick);

communicate(X, Y, Status, Neighbour, W, H, Tick) ->  
    case Neighbour of
        0 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y+1)); %NW
        1 -> Name = list_to_atom(integer_to_list(X) ++ integer_to_list(Y+1));   %N
        2 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y+1)); %NE
        3 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y));   %W
        4 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y));   %E
        5 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y-1)); %SW
        6 -> Name = list_to_atom(integer_to_list(X) ++ integer_to_list(Y-1));   %S
        7 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y-1))  %SE
    end,

    %io:format("I am cell ~p~p, sending my status (~p) to cell ~p~n", [X, Y, Status, Name]),
    Name ! {self(), {st_sent, X, Y, Status, Tick}}, % TODO: Coordinates only for debug
    communicate(X, Y, Status, Neighbour+1, W, H, Tick).



% Applies the rules and calculates the next state according 
% to the neighborhood and the current status.
calculate_future(Alive_count, 'dead') when (Alive_count == 3) -> 'alive';
calculate_future(_, 'dead') -> 'dead';
calculate_future(Alive_count, 'alive') when (Alive_count < 2) or (Alive_count > 3) -> 'dead';
calculate_future(Alive_count, 'alive') when (Alive_count == 2) or (Alive_count == 3) -> 'alive'.
