-module(controller).
-behaviour(gen_server).

%% API
-export([start_controller/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


start_controller() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

init(_) ->
    
    %controller init
    global:register_name(controller,self()),
    % Start additional servers on remote nodes
    Node1 = 'a@127.0.0.1',
    Node2 = 'b@127.0.0.1',
    Node3 = 'c@127.0.0.1',
    Node4 = 'd@127.0.0.1',
    BORDERS_1 = [{XMIN_1,XMAX_1,YMIN_1,YMAX_1,tower1,Controller_PID}] =[{0, 400,0,400,tower1,self()}],
    BORDERS_2 = [{XMIN_2,XMAX_2,YMIN_2,YMAX_2,tower2,Controller_PID}] =[{401, 800,0,400,tower2,self()}],
    BORDERS_3 = [{XMIN_3,XMAX_3,YMIN_3,YMAX_3,tower3,Controller_PID}] =[{0, 400,401,800,tower3,self()}],
    BORDERS_4 = [{XMIN_4,XMAX_4,YMIN_4,YMAX_4,tower4,Controller_PID}] =[{401, 800,401,800,tower4,self()}],
    ETS_1 = ets:new(ets1,[set,named_table]),
    ETS_2 = ets:new(ets2,[set,named_table]),
    ETS_3 = ets:new(ets3,[set,named_table]),
    ETS_4 = ets:new(ets4,[set,named_table]),
    {ok,PID_1} = rpc:call(Node1, tower, start_tower, BORDERS_1),
    {ok,PID_2} = rpc:call(Node2, tower, start_tower, BORDERS_2),
    {ok,PID_3} = rpc:call(Node3, tower, start_tower, BORDERS_3),
    {ok,PID_4} = rpc:call(Node4, tower, start_tower, BORDERS_4),
    %LETS CHECK IF THEY ARE ALIVE
    P1_Alive = rpc:call(Node1, erlang, is_process_alive, [PID_1]),
    P2_Alive = rpc:call(Node2, erlang, is_process_alive, [PID_2]),
    P3_Alive = rpc:call(Node3, erlang, is_process_alive, [PID_3]),
    P4_Alive = rpc:call(Node4, erlang, is_process_alive, [PID_4]),
    io:format("Process 1 is ~p~n",[P1_Alive]),
    io:format("Process 2 is ~p~n",[P2_Alive]),
    io:format("Process 3 is ~p~n",[P3_Alive]),
    io:format("Process 4 is ~p~n",[P4_Alive]),
    REF_1 = erlang:monitor(process,PID_1),
    REF_2 = erlang:monitor(process,PID_2),
    REF_3 = erlang:monitor(process,PID_3),
    REF_4 = erlang:monitor(process,PID_4),
    io:format("The pids of the referenced processes are ~p ~p ~p ~p ~n",[PID_1,PID_2,PID_3,PID_4]),
    TimerInterval = 500, % milliseconds
    erlang:send_after(TimerInterval, self(), {send_to_graphics}),
    %{ok,State} where State consists of [PID_i,REF_i,ETS_i,XMIN_i,XMAX_i,YMIN_i,YMAX_i]
    State = [{PID_1,REF_1,ETS_1,XMIN_1,XMAX_1,YMIN_1,YMAX_1},{PID_2,REF_2,ETS_2,XMIN_2,XMAX_2,YMIN_2,YMAX_2},{PID_3,REF_3,ETS_3,XMIN_3,XMAX_3,YMIN_3,YMAX_3},{PID_4,REF_4,ETS_4,XMIN_4,XMAX_4,YMIN_4,YMAX_4}],
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call({ask_help,_Tower_Pid,Airplane_Info}, _From, State) ->
    io:format("[Controller] - someone asks for help ~n"),
    [{Airplane_PID,Type,X,Y,Z,Angle,Speed}] = Airplane_Info,
    New_Tower_Index = find_right_tower(State,X,Y),
    case New_Tower_Index of
        none -> 
            io:format("Spinning ~n"),
            {reply,{spin,Airplane_PID},State};

        Index -> 
            Next_Tower = lists:nth(Index,State),
            io:format("Giving to index ~p a new plane ~n",[Next_Tower]),
            {PID_Next,_REF_Next,_ETS_Next,_XMIN_Next,_XMAX_Next,_YMIN_Next,_YMAX_Next} = Next_Tower,
            gen_server:cast(PID_Next,{take_charge,self(),[Type,X,Y,Z,Angle,Speed]}),
        {reply,{destroy,Airplane_PID},State}
    end;
        
handle_call(_Request, _From, State) ->
    io:format("hello my friends ~n",[]),
    {reply, ok, State}.


handle_cast({From,ETS_as_list}, State) ->
    {ok,File_Pointer} = file:open("received_ets.txt",[write]),
    % io:format("Received an ETS from ~p, ETS contents: ~p ~n",[From,ETS_as_list]),
    file:write(File_Pointer,"Received a message about ETS"),
    file:close(File_Pointer),
    % message about new ETS contents
    [{PID_1,_REF_1,_ETS_1,_XMIN_1,_XMAX_1,_YMIN_1,_YMAX_1},{PID_2,_REF_2,_ETS_2,_XMIN_2,_XMAX_2,_YMIN_2,_YMAX_2},{PID_3,_REF_3,_ETS_3,_XMIN_3,_XMAX_3,_YMIN_3,_YMAX_3},{PID_4,_REF_4,_ETS_4,_XMIN_4,_XMAX_4,_YMIN_4,_YMAX_4}] = State,
    case From of
        PID_1 ->
            ets:delete_all_objects(ets1),
            lists:foreach(fun(Elem) -> ets:insert(ets1, Elem) end, ETS_as_list),
            Newstate = State;
        PID_2 ->
            ets:delete_all_objects(ets2),
            lists:foreach(fun(Elem) -> ets:insert(ets2, Elem) end, ETS_as_list),
            Newstate = State;
        PID_3 ->
            ets:delete_all_objects(ets3),
            lists:foreach(fun(Elem) -> ets:insert(ets3, Elem) end, ETS_as_list),
            Newstate = State;

        PID_4 ->
            ets:delete_all_objects(ets4),
            lists:foreach(fun(Elem) -> ets:insert(ets4, Elem) end, ETS_as_list),
            Newstate = State
                end,
    {noreply, Newstate};
handle_cast(A,State)->
    io:format("A = ~p, B = ~p ~n",[A,State]),
    {noreply,State}.

handle_info({send_to_graphics},State) ->

    % Reschedule the timer
    send_to_graphics(),
    TimerInterval = 150, % milliseconds
    erlang:send_after(TimerInterval, self(), {send_to_graphics}),
    {noreply,  State};


handle_info(_Info, State) ->
    {ok,FD} = file:open("handleinfo_controller.txt",[append]),
    io:format("[COntroller] Handle INFO ~p",[_Info]),
    file:write(FD,_Info),
    file:write(FD,"nignig ~n"),
    file:close(FD),
    %io:format("The message is ~s",[_Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
send_to_graphics()->
    % send the current ETS tables to the graphics module
    %pid: type, x,y,z ,angle , speed
    %{graphics, 'py@127.0.0.1'} ! {ets1,[{123,airplane1,50,700,random:uniform(2000),90,random:uniform(10)},{456,airplane2,700,50,random:uniform(2000),180,random:uniform(10)}], [self()]},
    {graphics, 'py@127.0.0.1'} ! {ets1 , ets:tab2list(ets1),[self()]},    
    {graphics, 'py@127.0.0.1'} ! {ets2 , ets:tab2list(ets2),[self()]},
    {graphics, 'py@127.0.0.1'} ! {ets3 , ets:tab2list(ets3),[self()]},
    {graphics, 'py@127.0.0.1'} ! {ets4 , ets:tab2list(ets4),[self()]}.

find_right_tower(State,X,Y)->
    [{_PID_1,_REF_1,_ETS_1,XMIN_1,XMAX_1,YMIN_1,YMAX_1},{_PID_2,_REF_2,_ETS_2,XMIN_2,XMAX_2,YMIN_2,YMAX_2},{_PID_3,_REF_3,_ETS_3,XMIN_3,XMAX_3,YMIN_3,YMAX_3},{_PID_4,_REF_4,_ETS_4,XMIN_4,XMAX_4,YMIN_4,YMAX_4}] = State,
    if
        X>XMIN_1 andalso X<XMAX_1 andalso Y>YMIN_1 andalso Y<YMAX_1 ->
            Res =1;
        X>XMIN_2 andalso X<XMAX_2 andalso Y>YMIN_2 andalso Y<YMAX_2 ->
            Res =2;
        X>XMIN_3 andalso X<XMAX_3 andalso Y>YMIN_3 andalso Y<YMAX_3 ->
            Res =3;
        X>XMIN_4 andalso X<XMAX_4 andalso Y>YMIN_4 andalso Y<YMAX_4 ->
            Res =4;
        true ->
            Res = none
    end,
    Res.
