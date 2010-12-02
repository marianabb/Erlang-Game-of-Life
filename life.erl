-module(life).
-export([create/0, init_life/5, new_cell/5, 
         cell_loop/6, check_neighbours/7, 
         calculate_future/2, communicate/6,
         calculate_max_n/5,
         unregister_all/2, message_test/0]).


-record(cell, {x, y, now_state, next_state = 'tbd'}).

% Just a Cell creation test
create() ->    
    X = #cell{x=1, y=2, now_state=alive, next_state=dead},
    io:format("x is: ~p", [X]).


% Function that receives the initial state and spawns a 
% process for every square. Every process must have a cell
% with coordinates and state.
% The last argument is a list of strings in which every string
% represents one row of the board.
% Example board: [" X ", " X ", " X "]
% Example call: life:init_life(3, 3, [" X ", " X ", " X "], 0, 0).
% TODO: Verify that Width and Height are correct
init_life(_, _, [], _, _) ->
    % Next-step calculation should probably begin here
    ok;
init_life(Width, Height, [Row | Board], N_row, _) when (Row == []) -> init_life(Width, Height, Board, N_row + 1, 0);
init_life(Width, Height, [ [X|XS] | Board], N_row, N_col) ->
    S_row = integer_to_list(N_row),
    S_col = integer_to_list(N_col),
    
    if
        X == 88 -> State = 'alive'; % 88 is ISO for X
        true -> State = 'dead'
    end,
    Pid = spawn(fun () -> new_cell(Width, Height, N_col, N_row, State) end),
    register(list_to_atom(S_col ++ S_row), Pid),
    init_life(Width, Height, [XS | Board], N_row, N_col + 1).


% Creates a new Cell with the first state and makes the
% process execute cell_loop.
new_cell(W, H, X, Y, State) ->    
    Cell = #cell{x = X, y = Y, now_state = State},
    io:format("New cell ~p created~n", [Cell]),
    % Calculate Max_neigh according to the coordinates
    if 
        (X =/= 0) and (Y =/= 0) and (X =/= W-1) and (Y =/= H-1) ->
            Max_neigh = 8;
        true ->
            Max_neigh = calculate_max_n(X, Y, W, H, 8)
    end,
    % TODO: Maybe I should wait a little before starting the loop...
    cell_loop(W, H, Cell, 0, Max_neigh, 0).

% Calculates the Maximum number of neighbours for a cell
% that is located in a border position.
calculate_max_n(X, Y, W, H, Max) ->
    if 
        (X == 0) -> calculate_max_n(1, Y, W, H, Max-3);
        (Y == 0) -> calculate_max_n(X, 1, W, H, Max-1);
        (X+1 == W) -> calculate_max_n(1, Y, W, H, Max-3);
        (Y+1 == H) -> calculate_max_n(Y, 1, W, H, Max-1);
        true -> Max
    end.    

% The function that controls the actions that every cell
% will perform based on the messages it receives.
cell_loop(W, H, Cell, Num_neigh, Max_neigh, Alive_count) ->
    io:format("I am cell ~p starting!!~n", [Cell]),

    % Send my status to all my neighbours in the beginning of every tick
    % We know that at the start of a tick Num_neigh == 0
    if (Num_neigh == 0) ->
            communicate(Cell#cell.x, Cell#cell.y, Cell#cell.now_state, 0, W, H)
    end,

    % Check if I have all my neighbors status. If I do, calculate my next status.
    if (Num_neigh == Max_neigh) ->
            Future = calculate_future(Alive_count, Cell#cell.now_state),
            case future =/= Cell#cell.now_state of
                true when Future == 'alive' -> 
                    frame ! {change_cell, Cell#cell.x, Cell#cell.y, purple}; %TODO: Don't I need a "printer"??
                true when Future == 'dead' ->
                    frame ! {change_cell, Cell#cell.x, Cell#cell.y, white} %TODO: Don't I need a "printer"?? White?
            end,
            cell_loop(W, H, {cell, Cell#cell.x, Cell#cell.y, 
                             Cell#cell.now_state, Future}, 
                      0, Max_neigh, 0) %TODO: Should CHANGE my status here! Do I really need next_state?
    end,
    
    receive 
%%         {From, {next}} -> %TODO; Not using From
%%             %io:format("I am cell ~p starting!!~n", [Cell]),
%%             {Dead_c, Alive_c} = check_neighbours(0, 0, 0, Cell#cell.x, Cell#cell.y, W, H),
%%             io:format("I am Cell ~p my counts are: ~p dead, ~p alive~n", [Cell, Dead_c, Alive_c]),
%%             io:format("I am Cell ~p, my future is: ~p~n", [Cell, calculate_future(Alive_c, Cell#cell.now_state)]),
%%             %calculate_future(Alive_c, Cell#cell.now_state),
%%             cell_loop(W, H, {cell, Cell#cell.x, Cell#cell.y, 
%%                              Cell#cell.now_state, calculate_future(Alive_c, Cell#cell.now_state)}, 
%%                       Num_neigh, Max_neigh, Alive_count);

%%         {Neighb, {send_st}} ->
%%             io:format("Sending my status to ~p~n", [Neighb]),
%%             Neighb ! {receive_st, Cell#cell.now_state}, % REVIEW: should I send self()?
%%             cell_loop(W, H, Cell, Num_neigh, Max_neigh, Alive_count);

        {Neighb, {st_sent, X, Y, N_status}} -> %TODO: remove coordinates!! Not using Neighb
            io:format("Received status ~p from neighbour ~p~p~n", [N_status, X, Y]),
            case N_status of
                'alive' -> cell_loop(W, H, Cell, Num_neigh+1, Max_neigh, Alive_count+1);
                'dead' -> cell_loop(W, H, Cell, Num_neigh+1, Max_neigh, Alive_count)
            end
    end.

message_test() ->
    '01' ! {self(), {next}},
    '01' ! {self(), {next}}.
  


% Establishes communication with all of the neighbours of
% the cell at position (X,Y) and counts the number of dead and
% alive neighbours.
% If the neighbour is outside the board we will say it is dead.
% TODO: Could be simplified if we could ignore Dead_count.
%% check_neighbours(8, Dead_count, Alive_count, _, _, _, _) ->
%%     {Dead_count, Alive_count};

%% check_neighbours(Neighbour, Dead_count, Alive_count, 0, Y, W, H) 
%%   when (Neighbour == 0) or (Neighbour == 3) or (Neighbour == 5) ->
%%     check_neighbours(Neighbour+1, Dead_count+1, Alive_count, 0, Y, W, H);

%% check_neighbours(Neighbour, Dead_count, Alive_count, X, 0, W, H) 
%%   when (Neighbour == 6) ->
%%     check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, 0, W, H);

%% check_neighbours(Neighbour, Dead_count, Alive_count, X, Y, W, H) 
%%   when (X+1 == W) ->
%%     case Neighbour of 
%%         2 -> check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, Y, W, H); %TODO: Better way to write this?
%%         4 -> check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, Y, W, H);
%%         7 -> check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, Y, W, H)
%%     end;

%% check_neighbours(Neighbour, Dead_count, Alive_count, X, Y, W, H) 
%%   when (Y+1 == H) ->
%%     case Neighbour of 
%%         1 -> check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, Y, W, H) %TODO: Better way to write this?
%%     end;

%% check_neighbours(Neighbour, Dead_count, Alive_count, X, Y, W, H) ->   
%%     case Neighbour of
%%         0 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y+1)); %NW
%%         1 -> Name = list_to_atom(integer_to_list(X) ++ integer_to_list(Y+1));   %N
%%         2 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y+1)); %NE
%%         3 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y));   %W
%%         4 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y));   %E
%%         5 -> Name = list_to_atom(integer_to_list(X-1) ++ integer_to_list(Y-1)); %SW
%%         6 -> Name = list_to_atom(integer_to_list(X) ++ integer_to_list(Y-1));   %S
%%         7 -> Name = list_to_atom(integer_to_list(X+1) ++ integer_to_list(Y-1))  %SE
%%     end,
%%     Name ! {self(), {send_st}}, %REVIEW: Should I use my PID or my name?
%%     io:format("I am cell ~p~p and I am waiting for status of ~p~n", [X, Y, Name]),
%%     receive
%%         {receive_st, Status} -> % REVIEW: should I receive Pid?
%%             io:format("I received status ~p from my neighbour ~p~n", [Status, Name]),
%%             case Status of
%%                 'dead' -> check_neighbours(Neighbour+1, Dead_count+1, Alive_count, X, Y, W, H);
%%                 'alive' -> check_neighbours(Neighbour+1, Dead_count, Alive_count+1, X, Y, W, H)
%%             end
%%     end.

% Sends the status of Cell to all its neighbours 
% If the neighbour is outside the board it will not be considered 
communicate(_, _, _, 8, _, _) ->
    io:format("I have sent my state to all my neighbours~n", []),
    ok;
communicate(0, Y, Status, Neighbour, W, H)
  when (Neighbour == 0) or (Neighbour == 3) or (Neighbour == 5) ->
    communicate(0, Y, Status, Neighbour+1, W, H);

communicate(X, 0, Status, Neighbour, W, H)
  when (Neighbour == 6) ->
    communicate(X, 0, Status, Neighbour+1, W, H);

communicate(X, Y, Status, Neighbour, W, H)
  when (X+1 == W) ->
    case Neighbour of
        2 -> communicate(X, Y, Status, Neighbour+1, W, H);    
        4 -> communicate(X, Y, Status, Neighbour+1, W, H);
        7 -> communicate(X, Y, Status, Neighbour+1, W, H)
    end;

communicate(X, Y, Status, Neighbour, W, H)
  when (Y+1 == H) ->
    case Neighbour of
        1 -> communicate(X, Y, Status, Neighbour+1, W, H)
    end;

communicate(X, Y, Status, Neighbour, W, H) ->  
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
    io:format("I am cell ~p~p, sending my status (~p) to cell ~p~n", [X, Y, Status, Name]),
    Name ! {self(), {st_sent, X, Y, Status}}, % REVIEW: Should I use my PID or my name? TODO: No coordinates!!
    communicate(X, Y, Status, Neighbour+1, W, H).



% Applies the rules and calculates the next state according 
% to the neighborhood and the current status.
calculate_future(Alive_count, 'dead') when (Alive_count == 3) -> 'alive';
calculate_future(_, 'dead') -> 'dead';
calculate_future(Alive_count, 'alive') when (Alive_count < 2) or (Alive_count > 3) -> 'dead';
calculate_future(Alive_count, 'alive') when (Alive_count == 2) or (Alive_count == 3) -> 'alive'.
                    
     

unregister_all(0, 0) -> ok;
unregister_all(W, 0) ->
    unregister_all(W-1, 3); % TODO: 3 cableado aqui!!!
unregister_all(W, H) ->
    S_W = integer_to_list(W-1),
    S_H = integer_to_list(H-1),
    io:format("unregistering ~p~p~n", [W-1, H-1]),
    unregister(list_to_atom(S_W ++ S_H)),
    unregister_all(W, H-1).
