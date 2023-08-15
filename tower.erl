-module(tower).
-author("Danideanaf").
% -import(ets, [new/2, tab2list/1]).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_tower/1]).
 -export([terminate/2, code_change/3]).




handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("The terminating reason is  ~p",[_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



init(Borders) ->
    erlang:process_flag(trap_exit, true),
    %Tower init
    io:format("~p", [Borders]),
    timer:sleep(1000),
    {Xmin, Xmax, Ymin, Ymax,_Tower_name,Controller_PID}= Borders,
    io:format("known nodes = ~p ~n",[erlang:nodes()]),
    % Controller_PID = global:whereis_name(controller),
    % io:format("The controller pid is ~p ~n",[Controller_PID]),
    % Controller_Alive = rpc:call(node(Controller_PID),erlang,is_process_alive,[Controller_PID]),
    % io:format("The controller alive? is ~p ~n",[Controller_Alive]),
    io:format("The borders for PID ~p are ~p ~n",[self(),Borders]),
    Table = ets:new(planes,[set,public, named_table]),
    StripBusy = 0,
    XStart = rand:uniform(Xmax - Xmin) + Xmin -1,
    XEnd = rand:uniform(Xmax - XStart) + XStart -1,
    YStart = rand:uniform(Ymax - Ymin) + Ymin-1,
    YEnd = rand:uniform(Ymax - YStart) + YStart-1,
    %XStart = (Xmax + Xmin)/2,
    %XEnd = Xmax,
    %YStart = (Ymax + Ymin)/2,
    %YEnd = Ymax,
    Strip = {XStart,XEnd,YStart,YEnd},
    State = {Table,Strip,{Xmin,Xmax,Ymin,Ymax},StripBusy,Controller_PID},
    %State = {Table,Strip,{0,100,0,100},StripBusy},
    TimerInterval = 1000,
    erlang:send_after(TimerInterval, self(), create_plane),
    erlang:send_after(TimerInterval, self(), send_to_controller),
    %io:format("blabla ~n"),
    {ok,State}.
    
start_tower(Borders) ->
    % Pattern match on the Borders tuple to extract the necessary values
    {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID} = Borders,
    gen_server:start({global, Towername}, ?MODULE, {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID}, []). % Pass initial state as a tuple
    


create_plane(State,create_plane) ->
    io:format("in create plane ~n"),
    Time = rand:uniform(6),
    TypeTmp = rand:uniform(2),
    case TypeTmp of
        1 ->
            Type = airplane1;
        2 ->
            Type = airplane1;
        _->
            Type = airplane3,
            ok
    end,
    {_,{X1,X2,Y1,Y2},_,_,_Controller_PID}=State,
    Zend = rand:uniform(16) + 5,
    Strip = {{X1,Y1,0},{X2,Y2,Zend}},
    Angle = rand:uniform(360)-1,
    %Angle = 0,
    Speed = rand:uniform(10),
    Pos = {X1,Y1,0},
    TmpPlane = spawn(fun() -> plane:start_link([takeoff,self(),Strip,Pos,Speed,Angle,Time]) end),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    timer:sleep(10000),
    io:format("The tower is not dead ~n"),
    %TimerInterval = 1000,
    %erlang:send_after(TimerInterval, self(), create_plane),
    {ok, TmpPlane};

create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge) ->
    Time = rand:uniform(6),
    {_,{X1,X2,Y1,Y2},_,_}=State,
    Strip = {{X1,Y1,0},{X2,Y2,Z}},
    Pos = {X,Y,Z},
    TmpPlane = spawn(fun() -> plane:start_link([flying,self(),Strip,Pos,Speed,Angle,Time]) end),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    {ok, TmpPlane}.

%grabs everything
% handle_info(Message,State)->
%     timer:sleep(10000),
%     io:format("Received: ~p~n", [Message]),
%     io:format("Received: ~p~n", [State]),
%     {noreply,State};

handle_info(create_plane,State) -> 
    {ok, _PlanePid} = create_plane(State,create_plane),
    {noreply,State};




handle_info({take_charge, Sender_PID, [Type, X,Y,Z,Angle,Speed]},State) -> 
    {_Table,_Strip,{_Xmin,_Xmax,_Ymin,_Ymax},_StripBusy,Controller_PID} = State,
case Sender_PID of
    Controller_PID ->
        {ok, _PlanePid} = create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge),
        {noreply, State};
    _ ->
        {noreply, State}
end;


handle_info(send_to_controller,State) ->
    %io:format("Before sleep ~n"),
    %tower handle info for send_to_controller message
    io:format("Sending to main controller ~p ~n",[self()]),
    % Controller_PID = global:whereis_name(controller),
    {_,_,_,_,Controller_PID} = State,
    io:format("The Controller PID is ~p ~n",[Controller_PID]),
    ETS_as_list = ets:tab2list(planes),
    gen_server:cast(Controller_PID,{self(),ETS_as_list}),
    TimerInterval = 1000,
    erlang:send_after(TimerInterval, self(), send_to_controller),
    timer:sleep(10000),
    {noreply,State};



handle_info({update, {X,Y,Z}, Angle, PlanePid},State) ->
    {Table,_Strip,{Xmin,Xmax,Ymin,Ymax},_StripBusy,Controller_PID} = State,
    {_,Type,_,_,_,_,Speed} = ets:lookup(Table, PlanePid),
    if
        X > Xmin, X < Xmax, Y > Ymin, Y < Ymax ->
            ets:insert({PlanePid,Type, X,Y,Z,Angle,Speed});% Code to execute when both X and Y are within the specified ranges
        true ->
            gen_server:call(Controller_PID,{self(),{X,Y,Z},PlanePid})% ask the controller what to do
    end,
    {noreply,State};


%plane requested to land
handle_info({land_req, {_X,_Y,_Z}, PlanePid},State) ->
    {_,Strip,_,Busy} = State,
    % Process the Request and generate a Reply
    if
        Busy == 0->
            gen_server:cast(PlanePid,{land_ack, yes, self(),Strip});
        true ->
            gen_server:cast(PlanePid,{land_ack, no, self(),Strip})
    end,
    {noreply,State};


handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format("Received DOWN message for process ~p with reason ~p~n", [Pid, Reason]),
    {noreply, State};


handle_info({'EXIT', FromPid, Reason}, State) ->
    io:format("Received EXIT message from ~p with reason ~p~n", [FromPid, Reason]),
    {noreply, State}.
    



handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State};


%off the grid borders
handle_call({off_limit_ack,PID}, _From, State) ->
    % Process the Request and generate a Reply
    gen_server:cast(PID,{self(),turn_around}),         % reply answer to plane
    {reply, ok, State};


%moved to another tower
handle_call({moved,PID}, _From, State) ->
    % Process the Request and generate a Reply
    erlang:exit(PID, moved),
    {reply, ok, State}.


