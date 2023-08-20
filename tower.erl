-module(tower).
-author("Danideanaf").
% -import(ets, [new/2, tab2list/1]).
-behaviour(gen_server).

%% API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([start_tower/1]).
 -export([terminate/2, code_change/3]).



 handle_cast({update, {X,Y,Z}, Angle, PlanePid},State) ->
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
                Call_Result = gen_server:call(Controller_PID,{ask_help,self(),[{_Airplane_PID,Type,X,Y,Z,Angle,_Speed}]}),
                case Call_Result of
                    
                    {spin,Airplane_PID} ->
                    
                        Return_Angle = snell({Xmin,Xmax,Ymin,Ymax},X,Y,Angle),  %calc new angle
                        gen_server:cast(Airplane_PID,{spin,Return_Angle}),
                        {noreply,State};
                    {destroy,Airplane_PID} ->
                        ets:delete(planes,Airplane_PID),
                        gen_statem:stop(Airplane_PID),
                        {noreply,State};
                    true ->
                        ok
                end
            end
        end,
        {noreply,State};
%plane requested to land
handle_cast({land_req, PlanePid},State) ->
    {_Table,Strip,_Bounds,Busy,Controller_PID} = State,
    % Process the Request and generate a Reply
    if
        Busy == 0->
            NewState = {_Table,Strip,_Bounds,1,Controller_PID},
            gen_server:cast(PlanePid,{land_ack,yes,Strip});
        true ->
            NewState = State,
            gen_server:cast(PlanePid,{land_ack,no,rand:uniform(20)+10})
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
    {_Table,_Strip,{_Xmin,_Xmax,_Ymin,_Ymax},_StripBusy,Controller_PID} = State,
case Sender_PID of
    Controller_PID ->
        K = create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge),
        {noreply, State};
    _ ->
        {noreply, State}
end;

handle_cast(_Msg, State) ->
    io:format("Bad handle cast - ~p ~n",[_Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



init(Borders) ->
    erlang:process_flag(trap_exit, true),
    %Tower init
    {Xmin, Xmax, Ymin, Ymax,_Tower_name,Controller_PID,ETS}= Borders,
    case lists:member(planes,ets:all()) of
        true ->
            Table = ets:whereis(planes),
            ets:delete_all_objects(planes);
        _Else_ ->Table = ets:new(planes,[set,public, named_table])
    end,
    lists:foreach(fun(Item) ->ets:insert(planes,Item) end,ETS),
    StripBusy = 0,
    XStart = Xmin+100,
    XEnd = Xmax-100,
    YStart = Ymin+100,
    YEnd = Ymax-100,
    Strip = {{XStart,YStart,0},{XEnd,YEnd,0}},
    State = {Table,Strip,{Xmin,Xmax,Ymin,Ymax},StripBusy,Controller_PID},
    TimerInterval = 100,
    erlang:send_after(TimerInterval, self(), create_plane),
    erlang:send_after(TimerInterval, self(), send_to_controller),
    {ok,State}.
    
start_tower(Borders) ->
    % Pattern match on the Borders tuple to extract the necessary values
    {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID,ETS} = Borders,
    global:unregister_name(Towername),
    gen_server:start({global, Towername}, ?MODULE, {Xmin, Xmax, Ymin, Ymax, Towername,Controller_PID,ETS}, []). % Pass initial state as a tuple


%creates a plane to takeoff
create_plane(State,create_plane) ->
    Time = rand:uniform(30),
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
    Speed = rand:uniform(10),
    Pos = {X1,Y1,0},
    Self= self(),
    {ok,TmpPlane} = plane:start_link([takeoff,Self,NewStrip,Pos,Speed,Angle,Time]),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    TimerInterval = 5000,
    erlang:send_after(TimerInterval, self(), create_plane),
    {ok, TmpPlane};


%creates plane from another tower
create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge) ->
    Time = rand:uniform(50),
    {_Table,Strip,_Borders,_StripBusy,_Controller_PID} = State,
    {{X1,Y1,_Tmp},{X2,Y2,_Tmp2}}= Strip,
    NewStrip = {{X1,Y1,0},{X2,Y2,Z}},
    Pos = {X,Y,Z},
    Self = self(),
    {ok,TmpPlane} = plane:start_link([flying,Self,NewStrip,Pos,Speed,Angle,Time]),
    ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
    {ok, TmpPlane}.

%message create plane
handle_info(create_plane,State) -> 
    {ok, _PlanePid} = create_plane(State,create_plane),
    {noreply,State};






%updates controller
handle_info(send_to_controller,State) ->
    {_,_,_,_,Controller_PID} = State,
    ETS_as_list = ets:tab2list(planes),
    gen_server:cast(Controller_PID,{self(),ETS_as_list}),
    TimerInterval = 150,
    erlang:send_after(TimerInterval, self(), send_to_controller),
    {noreply,State};






%plane requested to land
handle_info({land_req, {_X,_Y,_Z}, PlanePid},State) ->
    {_,Strip,_,Busy} = State,
    % Process the Request and generate a Reply
    if
        Busy == 0->
            gen_server:cast(PlanePid,{land_ack, yes, self(),Strip});        %isnt busy
        true ->
            gen_server:cast(PlanePid,{land_ack, no, self(),Strip})          %busy
    end,
    {noreply,State};


handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    io:format("Received DOWN message for process ~p with reason ~p~n", [Pid, Reason]),
    {noreply, State};


handle_info({'EXIT', FromPid, Reason}, State) ->
    io:format("Received EXIT message from ~p with reason ~p~n", [FromPid, Reason]),
    {noreply, State};


%grabs everything
handle_info(Message,State)->
    {Atom,PlanePid} = Message,
    if
        Atom == landed -> 
            ets:delete(planes, PlanePid),
            {_Table,Strip,_Bounds,_Busy,Controller_PID} = State,
            NewState = {_Table,Strip,_Bounds,0,Controller_PID},
            gen_server:cast(PlanePid,{plane_landed});
        true -> NewState = State
        end,
    {noreply,NewState}.
    



handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};




%off the grid borders
handle_call({off_limit_ack,PID}, _From, State) ->
    % Process the Request and generate a Reply
    gen_server:cast(PID,{self(),turn_around}),         % reply answer to plane
    {reply, ok, State};


%moved to another tower
handle_call({moved,PID}, _From, State) ->
    % Process the Request and generate a Reply
    erlang:exit(PID, moved),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.


 snell({_Xmin,_Xmax,_Ymin,_Ymax}, _X, _Y, Angle) ->           %returns an angle
    NewAngle =trunc(Angle +180 +rand:uniform(30))rem 360,
    NewAngle. 
