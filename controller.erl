-module(controller).
-behaviour(gen_server).

%% API
-export([start_controller/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_controller() ->
    gen_server:start({global, ?MODULE}, ?MODULE, [], []).

init(_) ->
    global:register_name(controller,self()),
    % Start additional servers on remote nodes
    Node1 = 'node1@hostname',
    Node2 = 'node2@hostname',
    Node3 = 'node3@hostname',
    Node4 = 'node4@hostname',
    BORDERS_1={XMIN_1,XMAX_1,YMIN_1,YMAX_1} ={0, 400,0,400},
    BORDERS_2 = {XMIN_2,XMAX_2,YMIN_2,YMAX_2} ={401, 800,0,400},
    BORDERS_3 = {XMIN_3,XMAX_3,YMIN_3,YMAX_3} ={0, 400,401,800},
    BORDERS_4 = {XMIN_4,XMAX_4,YMIN_4,YMAX_4} ={401, 800,401,800},
    ETS_1 = ets:new(ets1,[set,named_table]),
    ETS_2 = ets:new(ets2,[set,named_table]),
    ETS_3 = ets:new(ets3,[set,named_table]),
    ETS_4 = ets:new(ets4,[set,named_table]),
    {ok,PID_1} = rpc:call(Node1, tower, start_tower, [BORDERS_1]),
    {ok,PID_2} = rpc:call(Node2, tower, start_tower, [BORDERS_2]),
    {ok,PID_3} = rpc:call(Node3, tower, start_tower, [BORDERS_3]),
    {ok,PID_4} = rpc:call(Node4, tower, start_tower, [BORDERS_4]),
    REF_1 = erlang:monitor(process,PID_1),
    REF_2 = erlang:monitor(process,PID_2),
    REF_3 = erlang:monitor(process,PID_3),
    REF_4 = erlang:monitor(process,PID_4),
    TimerInterval = 500, % milliseconds
    erlang:send_after(TimerInterval, self(), {send_to_graphics}),
    %{ok,State} where State consists of [PID_i,REF_i,ETS_i,XMIN_i,XMAX_i,YMIN_i,YMAX_i]
    State = [{PID_1,REF_1,ETS_1,XMIN_1,XMAX_1,YMIN_1,YMAX_1},{PID_2,REF_2,ETS_2,XMIN_2,XMAX_2,YMIN_2,YMAX_2},{PID_3,REF_3,ETS_3,XMIN_3,XMAX_3,YMIN_3,YMAX_3},{PID_4,REF_4,ETS_4,XMIN_4,XMAX_4,YMIN_4,YMAX_4}],
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    io:format("hello my friends ~n",[]),
    {reply, ok, State}.

handle_cast({From,ETS_as_list}, State) ->
    % message about new ETS contents
    [{PID_1,_REF_1,_ETS_1,_XMIN_1,_XMAX_1,_YMIN_1,_YMAX_1},{PID_2,_REF_2,_ETS_2,_XMIN_2,_XMAX_2,_YMIN_2,_YMAX_2},{PID_3,_REF_3,_ETS_3,_XMIN_3,_XMAX_3,_YMIN_3,_YMAX_3},{PID_4,_REF_4,_ETS_4,_XMIN_4,_XMAX_4,_YMIN_4,_YMAX_4}] = State,
    case From of
        PID_1 ->
            ets:delete_all_objects(ets1),
            lists:foreach(ETS_as_list,fun(Elem) -> ets:insert(ets1, Elem), ok end, ETS_as_list),
            Newstate = State;
        PID_2 ->
            ets:delete_all_objects(ets2),
            lists:foreach(ETS_as_list,fun(Elem) -> ets:insert(ets2, Elem), ok end, ETS_as_list),
            Newstate = State;
        PID_3 ->
            ets:delete_all_objects(ets3),
            lists:foreach(ETS_as_list,fun(Elem) -> ets:insert(ets3, Elem), ok end, ETS_as_list),
            Newstate = State;

        PID_4 ->
            ets:delete_all_objects(ets4),
            lists:foreach(ETS_as_list,fun(Elem) -> ets:insert(ets4, Elem), ok end, ETS_as_list),
            Newstate = State
                end,
    {noreply, Newstate}.

handle_info({send_to_graphics},State) ->
    % Reschedule the timer
    send_to_graphics(),
    TimerInterval = 500, % milliseconds
    erlang:send_after(TimerInterval, self(), {send_to_graphics}),
    {noreply,  State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
send_to_graphics()->
    % send the current ETS tables to the graphics module
    %pid: type, x,y,z ,angle , speed
    {graphics, 'py@127.0.0.1'} ! {ets1,[{123,airplane1,50,700,random:uniform(2000),90,random:uniform(10)},{456,airplane2,700,50,random:uniform(2000),180,random:uniform(10)}], [self()]},
    {graphics, 'py@127.0.0.1'} ! {ets2 , ets:tab2list(ets2),[self()]},
    {graphics, 'py@127.0.0.1'} ! {ets3 , ets:tab2list(ets3),[self()]},
    {graphics, 'py@127.0.0.1'} ! {ets4 , ets:tab2list(ets4),[self()]}.

