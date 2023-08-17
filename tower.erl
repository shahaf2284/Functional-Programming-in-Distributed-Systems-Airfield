-module(tower).
-author("Danideanaf").
% -import(ets, [new/2, tab2list/1]).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_tower/1]).
 -export([terminate/2, code_change/3]).



 handle_cast({update, {X,Y,Z}, Angle, PlanePid},State) ->
    %io:format("GETTING NEW COORDINATES from plane, new coordinates = ~p,~p,~p~n",[X,Y,Z]),
    % io:format("The state in getting... is ~p ~n",[State]),
    {_Table,_Strip,{Xmin,Xmax,Ymin,Ymax},_StripBusy,Controller_PID} = State,
    Lookup_Res = ets:lookup(planes,PlanePid),
    case Lookup_Res of 
        []->{noreply,State};
        [{_Airplane_PID,Type,_X,_Y,_Z,_Angle,_Speed}]->
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
                    {destroy,Airplane_PID} ->
                        io:format("[Tower] - Destroy, Airplanepid = ~p ~n",[Airplane_PID]),
                        ets:delete(planes,Airplane_PID),
                        io:format("[Tower] - line 34 ~n"),
                        gen_statem:stop(Airplane_PID),
                        io:format("[Tower] line 36 ~n"),
                        {noreply,State};
                        % gen_server:cast(Airplane_PID,{destroy})
                    true ->
                        io:format("[Tower] true = ~p",[Call_Result])
                end
            end
        end,
        {noreply,State};
%plane requested to land
handle_cast({land_req, PlanePid},State) ->
    %io:format("in land req for plane: ~p with reason ~n", [PlanePid]),
    {_Table,Strip,_Bounds,Busy,Controller_PID} = State,
    Time = rand:uniform(6),
    % Process the Request and generate a Reply
    if
        Busy == 0->
            NewState = {_Table,Strip,_Bounds,1,Controller_PID},
            gen_server:cast(PlanePid,{land_ack, yes,Time});
        true ->
            NewState = State,
            gen_server:cast(PlanePid,{land_ack, no,Time})
    end,
    {noreply,NewState};

%kill the plane when landed 
handle_cast({landed, PlanePid},State) ->
    ets:delete(planes, PlanePid),
    {_Table,Strip,_Bounds,_Busy,Controller_PID} = State,
    NewState = {_Table,Strip,_Bounds,0,Controller_PID},
    gen_server:cast(PlanePid,{plane_landed}),
    {noreply,NewState};


handle_cast({take_charge, Sender_PID, [Type, X,Y,Z,Angle,Speed]},State) -> 
    io:format("[Tower] - handle cast take charge ~n"),
    {_Table,_Strip,{_Xmin,_Xmax,_Ymin,_Ymax},_StripBusy,Controller_PID} = State,
case Sender_PID of
    Controller_PID ->
        K = create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge),
        io:format("[Tower take charge handle cast] K=~p ~n",[K]),
        {noreply, State};
    _ ->
        io:format("[Tower] in handle cast nigga ~n"),
        {noreply, State}
end;

handle_cast(_Msg, State) ->
    io:format("Bad handle cast - ~p ~n",[_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("[Tower]-The terminating reason is  ~p",[_Reason]),
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
    % XStart = rand:uniform(Xmax - Xmin) + Xmin -1,
    % XEnd = rand:uniform(Xmax - XStart) + XStart -1,
    % YStart = rand:uniform(Ymax - Ymin) + Ymin-1,
    % YEnd = rand:uniform(Ymax - YStart) + YStart-1,
    XStart = Xmin+100,
    XEnd = Xmax-100,
    YStart = Ymin+100,
    YEnd = Ymax-100,
    %Strip = {XStart,XEnd,YStart,YEnd},
    Strip = {{XStart,YStart,0},{XEnd,YEnd,0}},
    State = {Table,Strip,{Xmin,Xmax,Ymin,Ymax},StripBusy,Controller_PID},
    %State = {Table,Strip,{0,100,0,100},StripBusy},
    io:format("[Tower] Name = ~p ~n",[_Tower_name]),
    % case _Tower_name of
    %     tower4 ->
    %         io:format("I am Tower 4, time to crash ~n"),
    %         timer:kill_after(20),
    %         {ok,State};
    %     Else ->
    %         ok
    %     end,
    TimerInterval = 100,
    erlang:send_after(TimerInterval, self(), create_plane),
    erlang:send_after(TimerInterval, self(), send_to_controller),
    %io:format("blabla ~n"),
    {ok,State}.
    
start_tower(Borders) ->
    % Pattern match on the Borders tuple to extract the necessary values
    {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID} = Borders,
    gen_server:start({global, Towername}, ?MODULE, {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID}, []). % Pass initial state as a tuple
start_tower(Borders,ETS)->
    %start a tower with an ets, as backup
    nigga.  


create_plane(State,create_plane) ->
    % io:format("in create plane ~n"),
    Time = rand:uniform(6),
    %Time = 100000,
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
    {_Table,Strip,_Borders,_StripBusy,_Controller_PID} = State,
    Zend = rand:uniform(16) + 5,
    {{X1,Y1,_Tmp},{X2,Y2,_Tmp2}}= Strip,
    NewStrip = {{X1,Y1,0},{X2,Y2,Zend}},
    Angle = rand:uniform(360)-1,
    %Angle = 0,
    Speed = rand:uniform(10),
    Pos = {X1,Y1,0},
    io:format("Self = ~p ~n",[self()]),
    Self= self(),
    {ok,TmpPlane} = plane:start_link([takeoff,Self,NewStrip,Pos,Speed,Angle,Time]),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    io:format("[Tower], temp plane[160] = ~p~n",[TmpPlane]),
    % TimerInterval = 5000,
    % erlang:send_after(TimerInterval, self(), create_plane),
    {ok, TmpPlane};

create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge) ->
    %Time = rand:uniform(6),
    Time = 5000,
    io:format("Creating plane because of take charge ~n"),
    {_Table,Strip,_Borders,_StripBusy,_Controller_PID} = State,
    {{X1,Y1,_Tmp},{X2,Y2,_Tmp2}}= Strip,
    NewStrip = {{X1,Y1,0},{X2,Y2,Z}},
    io:format("[Tower] - in create plane(take charge)"),
    Pos = {X,Y,Z},
    Self = self(),
    {ok,TmpPlane} = plane:start_link([flying,Self,NewStrip,Pos,Speed,Angle,Time]),
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
    TimerInterval = 150,
    erlang:send_after(TimerInterval, self(), send_to_controller),
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


 snell({Xmin,Xmax,Ymin,Ymax}, X, Y, Angle) ->
    NewAngle =trunc(Angle +180 +rand:uniform(30))rem 360,
    NewAngle. 
