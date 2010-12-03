%%%-------------------------------------------------------------------
%%% File    : frame.erl
%%% Author  : Sven-Olof Nystrom <svenolof.nystrom-nospam@bredband.net>
%%% Description : 
%%%
%%% Created : 24 Oct 2010 by Sven-Olof Nystrom <>
%%%-------------------------------------------------------------------
-module(frame).

%-include_lib("inets/src/httpd.hrl").
-include_lib("/usr/local/lib/erlang/lib/inets-5.5/src/http_server/httpd.hrl").
%-compile(export_all).
-export([init/0, do/1, loop/5]).

-export([draw/0, set_head/1, set_foot/1, set_w/1, set_h/1, 
         change_cell/3, reset_cells/0]).

%%%% from automata.erl. httpd glue.

init() ->
    process_flag(trap_exit, true),
    ok = ensure_started(inets),
    init_frame(),
    ServerConf =
	[{port, 8088},
	 {server_name, "Cellular Automata"},
	 {bind_address, {127, 0, 0, 1}},
	 {server_root, "."},
	 {document_root, "."},
	 {modules, [?MODULE]}
	],
    {ok, _Pid} = inets:start(httpd, ServerConf, inets),
    {ok, true}.

%%====================================================================
%% Inets HTTPD callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: do(Request) -> Page
%% Description: 
%%--------------------------------------------------------------------
-spec do(#mod{}) ->
    {break, [{response, {response, [{atom(), any()}], string()}}]}.
%%--------------------------------------------------------------------
do(#mod{method = Method, request_uri = URI, entity_body = Body}) ->
    case catch do_page(Method, URI, Body) of
	{ok, Type, Page} -> ok(Type, Page);
	{error, Code, Page} -> error(Code, Page);
	missing -> missing("Page not found");
	Error -> error(501, Error)
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
do_page("GET", "/", _) ->
    Page = frame:draw(),
    {ok, "text/html", expand(Page, [])};
do_page(_, _, _) ->
    {error, 400, ""}.

ensure_started(Application) ->
    case lists:keymember(Application, 1, application:which_applications()) of
	true -> ok;
	false -> application:start(Application)
    end.

error(Code, Error) -> break(Code, lists:flatten(io_lib:format("~p", [Error]))).
missing(Content) -> break(404, Content).
ok(Type, Content) -> break(200, Type, Content).
    

break(Code, Type, Content) ->
%    io:format("Content:~p~n", [Content]),
    Header = [{code, Code},
	      {content_type, Type},
	      {server, "Cellular Automata"},
	      {content_length, integer_to_list(length(Content))}],
    {break, [{response, {response, Header, Content}}]}.

break(Code, Content) ->
    Header = [{code, Code},
	      {content_type, "text/plain"},
	      {server, "Cellular Automata"},
	      {content_length, integer_to_list(length(Content))}],
    {break, [{response, {response, Header, Content}}]}.


%% unexpected(State, Event) ->
%%     error_logger:info_msg("An unexpected event:~p to: ~p of type ~p "
%% 			  "in state:~p~n",
%% 			  [Event, self(), ?MODULE, State]).

expand([], Acc) -> lists:flatten(lists:reverse(Acc));
expand([[] | T], Acc) -> expand(T, Acc);
expand([{html, H} | T], Acc) -> expand(T, [H | Acc]);
expand([{ehtml, E} | T], Acc) -> expand(T, [ehtml:ehtml_expand(E) | Acc]).






%%%% Frame

init_frame() ->
    io:format("Init frame~n",[]),
    register(frame, 
             spawn_link(frame, loop, ["", "", 0, 0, gb_trees:empty()])),
    default_setup().

default_setup() ->
    set_w(20),
    set_h(10),
    set_head("Simple stupid frame"),
    set_foot("Dummies: 20 - Smarties: 0").

cellpadding() ->
    10.

draw() ->
    Frame = whereis(frame),
    Frame ! {draw, self()},
    receive 
        {Frame, drawing, Table} ->
            Table
    end.

% Interface

set_head(Head) ->
    frame ! {set_head, Head}.

set_foot(Foot) ->
    frame ! {set_foot, Foot}.

set_w(W) ->
    frame ! {set_w, W}.

set_h(H) ->
    frame ! {set_h, H}.

change_cell(X, Y, Value) ->
    frame ! {change_cell, X, Y, Value}.

reset_cells() ->
    frame ! reset_cells.

% Main loop

loop(Head, Foot, W, H, Data) ->
    receive
        {set_head, Head1} ->
            frame:loop(Head1, Foot, W, H, Data);
        {set_foot, Foot1} ->
            frame:loop(Head, Foot1, W, H, Data);
        {set_w, W1} ->
            frame:loop(Head, Foot, W1, H, Data);
        {set_h, H1} ->
            frame:loop(Head, Foot, W, H1, Data);
        {change_cell, X1, Y1, Value} ->
            Data1 = gb_trees:enter({X1, Y1}, Value, Data),
            frame:loop(Head, Foot, W, H, Data1);
        reset_cells ->
            frame:loop(Head, Foot, W, H, gb_trees:empty());
        {draw, P} ->
            P ! {self(), drawing, draw(Head, Foot, W, H, Data)},
            frame:loop(Head, Foot, W, H, Data)
    end.


draw(Head, Foot, W, H, Data) ->
    draw_frame(Head, Foot, 
               draw_table(W, H, Data)).

draw_frame(Head, Foot, Body) ->
    [{ehtml,
      {html, [], 
       [{head, [],
         [{title, [], Head},
          {meta, [{'http-equiv',refresh},{content,1}], []}]},
        [Body, {p, [], Foot}]]}}].

draw_table(W, H, Data) ->
    {table, [{cellpadding,10},{cellspacing,"0"}], 
     draw_rows(0, W, H, Data)}.

draw_rows(Y, _W, H, _Data) when Y==H ->
    [];
draw_rows(Y, W, H, Data) when Y<H ->
    R = draw_row(Y, W, H, Data),
    Rs = draw_rows(Y+1, W, H, Data),
    [R|Rs].

draw_row(Y, W, H, Data) ->
    {tr, [], draw_items(0, Y, W, H, Data)}.

draw_items(X, _Y, W, _H, _Data) when X==W ->
    [];
draw_items(X, Y, W, H, Data) when X<W ->
    [draw_item(X,Y, Data) | 
     draw_items(X+1, Y, W, H, Data)].

draw_item(X, Y, Data) ->
    case gb_trees:lookup({X, Y}, Data) of
        none ->
            {td, ["?"]};
        {value, Color} ->
            {td, [{bgcolor, Color}]}
    end.
                
