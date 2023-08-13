module(tower).
-author("Danideanaf").
-import(ets, [new/2, tab2list/1]).
-behaviour(gen_server).
-export([init/1,start_tower/1]).
-define(SERVER, ?MODULE).


init(Borders) ->
    {Xmin,Xmax,Ymin,Ymax} = Borders,
    Table = ets:new(planes, [set,public, named_table]),
    XStart = rand:uniform(Xmin, Xmax),
    XEnd = rand:uniform(XStart, Xmax),
    YStart = rand:uniform(Ymin, Ymax),
    YEnd = rand:uniform(YStart, Ymax),
    Strip = {XStart,XEnd,YStart,YEnd},
    State = {Table,Strip,Borders},
    timer:send_interval(1000, {self(), create_plane}),
    timer:send_interval(1000, {self(), send_to_controller}), %% Start the timer for plane creation
    {ok,State}.
    
start_tower(Borders) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Borders], []).
    


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
    {_,{X1,X2,Y1,Y2},_}=State,
    Zend = rand:uniform(16) + 5,
    Strip = {{X1,Y1,0},{X2,Y2,Zend}},
    Angle = rand:uniform(360),
    Speed = rand:uniform(10) + 1,
    Pos = {X1,Y1,0},
    TmpPlane = spawn_link(fun() -> plane:start_link([takeoff,self(),Strip,Pos,Speed,Angle,Time]) end),
    ets:insert_new(TmpPlane,Type, X1,Y1,0,Angle,Speed),
    timer:send_interval(1000, {self(), create_plane}),
    {ok, TmpPlane}.

handle_info({Sender_PID, create_plane},State) -> 
    {Table,Strip,Borders} = State,
    Controller_PID = global:whereis_name(controller),
    MyPID = self(),
    case Sender_PID of
        MyPID ->
            {ok, PlanePid} = create_plane(State,create_plane),
            {noreply, State};
        Controller_PID ->
            {ok, PlanePid} = create_plane(State,take_charge),
            {noreply, State}
    end;


handle_info({PID, send_to_controller},State) ->
    Controller_PID = global:whereis_name(controller),
    ETS_as_list = tab2list(planes),
    gen_server:cast(Controller_PID,{self(),ETS_as_list}),
    timer:send_interval(1000, {self(), send_to_controller}).
    
