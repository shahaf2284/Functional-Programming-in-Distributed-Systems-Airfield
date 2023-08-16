-module(tower).
-author("Danideanaf").
% -import(ets, [new/2, tab2list/1]).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_tower/1]).
 -export([terminate/2, code_change/3]).



 handle_cast({update, {X,Y,Z}, Angle, PlanePid},State) ->
    io:format("GETTING NEW COORDINATES from plane, new coordinates = ~p,~p,~p~n",[X,Y,Z]),
    % io:format("The state in getting... is ~p ~n",[State]),
    {_Table,_Strip,{Xmin,Xmax,Ymin,Ymax},_StripBusy,Controller_PID} = State,
    [{_Airplane_PID,Type,_X,_Y,_Z,_Angle,_Speed}] = ets:lookup(planes, PlanePid),
    if
        X > Xmin andalso X < Xmax andalso Y > Ymin andalso Y < Ymax ->
            ets:insert(planes,{PlanePid,Type, X,Y,Z,Angle,_Speed});% Code to execute when both X and Y are within the specified ranges
        true ->
            %we are outside out square
            ets:insert(planes,{PlanePid,Type, X,Y,Z,Angle,_Speed}),
            io:format("[Tower] - Call result ~n"),
            Call_Result = gen_server:call(Controller_PID,{ask_help,self(),[{_Airplane_PID,Type,X,Y,Z,Angle,_Speed}]}),
            case Call_Result of
                
                {spin,Airplane_PID} ->io:format("[Tower] - Spin ~n"),
                Return_Angle = snell({Xmin,Xmax,Ymin,Ymax},X,Y,Angle),
                gen_server:cast(Airplane_PID,{spin,Return_Angle}),
                {noreply,State};
                {destory,Airplane_PID} ->
                    io:format("[Tower] - Destroy ~n"),
                    ets:delete(planes,Airplane_PID),
                    gen_server:cast(Airplane_PID,{destroy})
            end
    end,
    {noreply,State};
%plane requested to land
handle_cast({land_req, {_X,_Y,_Z}, PlanePid},State) ->
    %io:format("in land req for plane: ~p with reason ~n", [PlanePid]),
    {_Table,Strip,_Bounds,Busy,Controller_PID} = State,
    % Process the Request and generate a Reply
    if
        Busy == 0->
            NewState = {_Table,Strip,_Bounds,1,Controller_PID},
            gen_server:cast(PlanePid,{land_ack, yes,Strip});
        true ->
            NewState = State,
            gen_server:cast(PlanePid,{land_ack, no,Strip})
    end,
    {noreply,NewState};

%kill the plane when landed 
handle_cast({landed, PlanePid},State) ->
    ets:delete(planes, PlanePid),
    {_Table,Strip,_Bounds,_Busy,Controller_PID} = State,
    NewState = {_Table,Strip,_Bounds,0,Controller_PID},
    exit(PlanePid, normal),
    {noreply,NewState};


handle_cast({take_charge, Sender_PID, [Type, X,Y,Z,Angle,Speed]},State) -> 
    io:format("[Tower] - handle cast charge ~n"),
    {_Table,_Strip,{_Xmin,_Xmax,_Ymin,_Ymax},_StripBusy,Controller_PID} = State,
case Sender_PID of
    Controller_PID ->
        {ok, _PlanePid} = create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge),
        {noreply, State};
    _ ->
        {noreply, State}
end;

handle_cast(_Msg, State) ->
    io:format("Bad handle cast - ~p ~n",[_Msg]),
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
    TimerInterval = 100,
    erlang:send_after(TimerInterval, self(), create_plane),
    erlang:send_after(TimerInterval, self(), send_to_controller),
    %io:format("blabla ~n"),
    {ok,State}.
    
start_tower(Borders) ->
    % Pattern match on the Borders tuple to extract the necessary values
    {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID} = Borders,
    gen_server:start({global, Towername}, ?MODULE, {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID}, []). % Pass initial state as a tuple
    


create_plane(State,create_plane) ->
    % io:format("in create plane ~n"),
    %Time = rand:uniform(6),
     Time = 100000,
    TypeTmp = rand:uniform(2),
    case TypeTmp of
        1 ->
            Type = airplane1;
        2 ->
            Type = airplane2;
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
    io:format("Self = ~p ~n",[self()]),
    Temp_self = self(),

    {ok,TmpPlane} = plane:start_link([takeoff,Temp_self,Strip,Pos,Speed,Angle,Time]),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    io:format("[Tower], temp plane[105] = ~p~n",[TmpPlane]),
    % TimerInterval = 1000,
    % erlang:send_after(TimerInterval, self(), create_plane),
    {ok, TmpPlane};

create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge) ->
    %Time = rand:uniform(6),
    Time = 5000,
    io:format("Creating plane because of take charge ~n"),
    {_,{X1,X2,Y1,Y2},_,_}=State,
    Strip = {{X1,Y1,0},{X2,Y2,Z}},
    Pos = {X,Y,Z},
    TmpPlane = plane:start_link([flying,self(),Strip,Pos,Speed,Angle,Time]),
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







handle_info(send_to_controller,State) ->
    %tower handle info for send_to_controller message
    %io:format("Sending to main controller  from tower:~p ~n",[self()]),
    {_,_,_,_,Controller_PID} = State,
    %io:format("The Controller PID is ~p ~n",[Controller_PID]),
    ETS_as_list = ets:tab2list(planes),
    gen_server:cast(Controller_PID,{self(),ETS_as_list}),
    TimerInterval = 50,
    erlang:send_after(TimerInterval, self(), send_to_controller),
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

snell({Xmin,Xmax,Ymin,Ymax}, X, Y, _Angle) ->
    if Xmin > X ->
        Res = rand:uniform(120) - 60;
    Xmax < X ->
        Res = rand:uniform(120) + 120;
    Ymin > Y ->
        Res = rand:uniform(120) + 210;
    Ymax < Y ->
        Res = rand:uniform(120) + 30;
    true ->
        Res = rand:uniform(120) % Default case
    end,
    Res.
