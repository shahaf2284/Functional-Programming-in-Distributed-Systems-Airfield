-module(plane).
-behaviour(gen_statem).

-export([stop/0, start_link/1]).
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

% States:
-export([takeoff/3,flying/3,landing_request/3]).
-record(plane,{pos,speed,dir,time,state,strip,tower,endStrip,speedup}).           


stop() ->
    gen_statem:stop(?MODULE).

start_link([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [Status,TowerPid,Strip,Pos,Speed,Dirvec,Time], []).

init([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->  % initialize plane when starting the program
    io:format("~n=========init==========~n"),
    erlang:process_flag(trap_exit,true) ,   
    Plane = #plane{pos= Pos, 
                  speed= Speed, 
                  dir= Dirvec, 
                  time= Time, 
                  state= Status, 
                  strip=Strip, 
                  tower= TowerPid,
                  endStrip={0,0,0},
                  speedup = 0}, 
                  io:format("~n=========done init==========~n"),
                  erlang:send_after(20, self(), {Status}),
    {ok,Status, Plane}.            % send to start state/first state

takeoff(info,{State},Plane = #plane{}) ->        % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
    io:format("~n~p~n",[Plane#plane.strip]),
    io:format("~n=========takeoff========~n"),
    {_Start,{Xend,Yend,Zend}} = Plane#plane.strip,
    io:format("~n~p~n",[get_dir(Plane#plane.strip)]),
    UpdatedPlane= travel(Plane,get_dir(Plane#plane.strip)),
    io:format("~n~p~n",[UpdatedPlane]),
    {X,Y,_Z}= UpdatedPlane#plane.pos,
    if (Xend < X) or (Yend < Y) -> POS={Xend,Yend,Zend}, NextState =flying;
       true -> POS= UpdatedPlane#plane.pos, NextState=State
       end,
    UpPlane = UpdatedPlane#plane{pos= POS, state=NextState},
    erlang:send_after(50, self(), {NextState}),
    io:format("~n=========done takeoff========~n"),
    {next_state,NextState,UpPlane}.

flying(info,{State}, Plane = #plane{}) ->               % send message to communication tower
    io:format("~n=========flying========~n"),
    io:format("~n~p~n",[Plane]),
    {X,Y,Z} = Plane#plane.pos,
    Dir = Plane#plane.dir,
    Xnew = trunc(X + Plane#plane.speed*math:cos(Dir)),
    Ynew = trunc(Y + Plane#plane.speed*math:sin(Dir)),
    Time = Plane#plane.time-1,
    gen_server:cast(Plane#plane.tower,{update,{Xnew,Ynew,Z},Dir,self()}),        % Send rower my new location
    if Time == 0 -> NextState =landing_request;
       true -> NextState =State
    end,
    UpdatedPlane=Plane#plane{pos={Xnew,Ynew,Z}, time=Time,state=NextState},
    erlang:send_after(50, self(), {NextState}),
    %io:format("~n~p~n",[UpdatedPlane]),
    io:format("~n=========done flying========~n"),
    {next_state, NextState,UpdatedPlane};


flying(cast,{land_ack, Ans, Data}, Plane = #plane{}) ->               % send message to communication tower
    io:format("~n=========get Ack from server========~n"),
    case Ans of 
        yes -> {X,Y,Z} = Plane#plane.pos,
            {{X1,Y1,Z1},EndStrip} = Data,
            Teta=(180* math:atan(abs(Y1-Y)/abs(X1 - X)))/math:pi(),
            UpdatedPlane= Plane#plane{dir=Teta,strip=Data, state=fly_to_strip, endStrip=EndStrip};
        no -> UpdatedPlane = Plane#plane{time=Data,state=flying}
    end,
    erlang:send_after(100, self(), {Plane#plane.state}),
    {next_state, Plane#plane.state,UpdatedPlane}.

landing_request(info,{State},Plane = #plane{})->                      % send message to communication tower
    io:format("~n=========landing request========~n"),
    gen_server:cast(Plane#plane.tower,{land_req, Plane#plane.pos, self()}),        % Send rower my new location
    UpdatedPlane = Plane#plane{time=5,state=landing_request},           % add time to wait untill get message landing 
    {next_state, flying, UpdatedPlane}.

% fly_to_strip({strip_land},Plane = #plane{})->
%     {_Start,{Xend,Yend,Zend}}=Plane#plane.strip, 
%     timer:sleep(500),
%     erlang:send_after(1, self(), {fly, Plane}),
%     {X,Y,_Z}=Plane#plane.pos,
%     Teta = 180*math:atan(abs(Yend-Y)/abs(Xend-X))/math:pi(),
%     UpdatedPlane = travel_to_location(Plane,Teta),
%     {Xnew,Ynew,Znew}=UpdatedPlane#plane.pos,
%     if ((Xend-5 =< Xnew) and (Xnew=<Xend+5)) or ((Yend-5 =< Ynew) and (Ynew=<Yend+5)) ->
%             NewTeta = 180*math:atan(abs(Yend-Y)/abs(Xend-X))/math:pi(),
%             UpdatedPlane1 = Plane#plane{dir=NewTeta,state=landing},            % add time to wait untill get message landing 
%             {next_state, landing, UpdatedPlane1};
%     true->  {next_state, fly_to_strip, UpdatedPlane}
%     end.

callback_mode() -> state_functions.
terminate(_Reason, _State, _Data) ->
    io:format("Terminate reason = ~p",[_Reason]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

get_dir({{X,Y,_Z},{X1,Y1,_Z1}}) ->
        math:atan((Y1-Y)/(X1-X)). 

%Teta in rad
travel(Plane,Teta)-> 
        {X,Y,Z} = Plane#plane.pos,
        Xnew = trunc(X + Plane#plane.speed*math:cos(Teta)),
        Ynew = trunc(Y + Plane#plane.speed*math:sin(Teta)),
        UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}},
        _TetaDegree = 180*Teta/math:pi(),
        gen_server:cast(Plane#plane.tower,{update,{Xnew,Ynew,Z},_TetaDegree,self()}),        % Send rower my new location
        % io:format("~n=========Teta ~p========~n",[Teta]),
        % io:format("~n=========send to server location X-~p Y-~p========~n",[Xnew,Ynew]),
        UpdatedPlane. 