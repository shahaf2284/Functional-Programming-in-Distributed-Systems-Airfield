    -module(tower).
    -author("Danideanaf").
    -import(ets, [new/2, tab2list/1,insert_new/2]).
    -behaviour(gen_server).

    %% API
    -export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
    -export([start_tower/1]).
    %-export([terminate/2, code_change/3]).




    handle_cast(_Msg, State) ->
        {noreply, State}.

    terminate(_Reason, _State) ->
        ok.

    code_change(_OldVsn, State, _Extra) ->
        {ok, State}.




    init(Borders) ->
        {ok,FD} = file:open("tower_init.txt",[write]),
        file:write(FD,"initiated a tower with an rpc call"),
        file:close(FD),
        [Xmin, Xmax, Ymin, Ymax] = Borders,
        Table = ets:new(planes, [set,public, named_table]),
        StripBusy = 0,
        % XStart = rand:uniform(Xmax-Xmin) + Xmin,
        % XEnd = rand:uniform(Xmax-XStart) + XStart,
        % YStart = rand:uniform(Ymax-Ymin) + Ymin,
        % YEnd = rand:uniform(Ymax-YStart) + YStart,
        XStart = 100,
        XEnd = 300,
        YStart = 100,
        YEnd = 300,
        Strip = {XStart,XEnd,YStart,YEnd},
        State = {Table,Strip,{Xmin,Xmax,Ymin,Ymax},StripBusy},
        timer:send_interval(1000, {self(), create_plane}),
        timer:send_interval(1000, {self(), send_to_controller}), %% Start the timer for plane creation
        {ok,State}.
        
    start_tower([Xmin, Xmax, Ymin, Ymax]) ->
        gen_server:start_link({local, ?MODULE}, ?MODULE, [Xmin, Xmax, Ymin, Ymax], []).
        


    create_plane(State,create_plane) ->
        Time = rand:uniform(6),
        TypeTmp = rand:uniform(2),
        case TypeTmp of
            0 ->
                Type = airplane1;
            1 ->
                Type = airplane2;
            _->
                Type = airplane3,
                ok
        end,
        {_,{X1,X2,Y1,Y2},_,_}=State,
        Zend = rand:uniform(16) + 5,
        Strip = {{X1,Y1,0},{X2,Y2,Zend}},
        Angle = rand:uniform(360),
        Speed = rand:uniform(10) + 1,
        Pos = {X1,Y1,0},
        TmpPlane = spawn_link(fun() -> plane:start_link([takeoff,self(),Strip,Pos,Speed,Angle,Time]) end),
        ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
        timer:send_interval(1000, {self(), create_plane}),
        {ok, TmpPlane};

    create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge) ->
        Time = rand:uniform(6),
        {_,{X1,X2,Y1,Y2},_,_}=State,
        Strip = {{X1,Y1,0},{X2,Y2,Z}},
        Pos = {X,Y,Z},
        TmpPlane = spawn_link(fun() -> plane:start_link([flying,self(),Strip,Pos,Speed,Angle,Time]) end),
        ets:insert_new(planes, {TmpPlane, Type, X1, Y1, 0, Angle, Speed}),
        {ok, TmpPlane}.

    handle_info({Sender_PID, create_plane},State) -> 
        MyPID = self(),
        case Sender_PID of
            MyPID ->
                {ok, _PlanePid} = create_plane(State,create_plane),
                {noreply, State};
            _ ->
                {noreply, State}
        end;




    handle_info({take_charge, Sender_PID, {Type, X,Y,Z,Angle,Speed}},State) -> 
    Controller_PID = global:whereis_name(controller),
    case Sender_PID of
        Controller_PID ->
            {ok, _PlanePid} = create_plane({State,[Type, X,Y,Z,Angle,Speed]},take_charge),
            {noreply, State};
        _ ->
            {noreply, State}
    end;


    handle_info({_PID, send_to_controller},State) ->
        {ok,FD} = file:open("towertocontroller.txt",[write]),
        file:write(FD,"sending to danny at handle info"),
        file:close(FD),
        Controller_PID = global:whereis_name(controller),
        ETS_as_list = tab2list(planes),
        gen_server:cast(Controller_PID,{self(),ETS_as_list}),
        timer:send_interval(1000, {self(), send_to_controller}),
        {noreply,State};



    handle_info({update, {X,Y,Z}, Angle, PlanePid},State) ->
        Controller_PID = global:whereis_name(controller),
        {Table,_,Borders,_} = State,
        {Xmin,Xmax,Ymin,Ymax} = Borders,
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
                en_server:cast(PlanePid,{land_ack, yes, self(),Strip});
            true ->
                en_server:cast(PlanePid,{land_ack, no, self(),Strip})
        end,
        {noreply,State}.
        



    handle_call(stop, _From, State) ->
        {stop, normal, stopped, State};

    handle_call(_Request, _From, State) ->
        {reply, ok, State};


    %off the grid borders
    handle_call({off_limit_ack,PID}, _From, State) ->
        % Process the Request and generate a Reply
        en_server:cast(PID,{self(),turn_around}),         % reply answer to plane
        {reply, ok, State};


    %moved to another tower
    handle_call({moved,PID}, _From, State) ->
        % Process the Request and generate a Reply
        erlang:exit(PID, moved),
        {reply, ok, State}.
