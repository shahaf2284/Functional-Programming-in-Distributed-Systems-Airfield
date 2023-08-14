-module(plane).
-behaviour(gen_statem).
-export([stop/0, start_link/0,start_link/1]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-export([takeoff/2]).
-record(plane,{pos,speed,dir,time,state,strip,tower,endStrip,speedup}).           
-define(SERVER,?MODULE).

stop() ->
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

start_link([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Status,TowerPid,Strip,Pos,Speed,Dirvec,Tim

init([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->  % initialize plane when starting the program
    Plane = #plane{pos= Pos, 
                  speed= Speed, 
                  dir= Dirvec, 
                  time= Time, 
                  state= Status, 
                  strip=Strip, 
                  tower= TowerPid,
                  endStrip={0,0,0}, 
                  speedup=0,  
                  speedDo=0}     
    {ok,Status, Plane}.                              % send to start state/first state


takeoff(_EventType,Plane = #plane{}) ->        % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
    {_Start,{Xend,Yend,Zend}} = Plane#plane.strip,
    spleep(500),
    erlang:send_after(1, self(), {update_takeoff, Plane}),
    {next_state, Plane#plane.state, UpdatedPlane};

flying({_EventType,Plane = #plane{}) ->               % send message to communication tower
    sleep(500)
    erlang:send_after(1, self(), {timeout, Plane}),
    if (Plane#plane.time = 0) -> NextStateName = landing_request;
        true-> NextStateName = flying 
    end,
    {next_state, NextStateName, Plane}.

landing_request(_EventType,Plane = #plane{})->                             % send message to communication tower
    gen_server:cast(?MODULE,{landing_request, Plane#plane.pos, self()}),   % Send rower my new location
    UpdatedPlane = Plane#plane{time=1000,state=landing_request},           % add time to wait untill get message landing 
    {next_state, flying, UpdatedPlane}.

fly_to_strip({strip_land},Plane = #plane{})->
    {Start,{Xend,Yend,_Z}}=Plane#plane.strip, 
    sleep(500)
    erlang:send_after(1, self(), {fly, Plane}),
    UpdatedPlane = travel_to_location(Plane,Teta),
    {X,Y,_Z}=Plane#plane.pos, 
    if ((Xend-5 =< Xnew) and (Xnew=<Xend+5)) or ((Yend-5 =< Ynew) and (Ynew=<Yend+5)) -> UpdatedPlane ->
         NextStateName = landing;
        true-> NextStateName = fly_to_strip 
    end,
    {next_state, NextStateName, Plane}.

landing({land},Plane=#plane{})->
    {Start,EndStrip}=Plane#plane.strip,
    Teta = get_dir(EndStrip, Start),
    UpdatedPlane = land_fuc(Plane,Teta,),
    {X,Y,Z} = EndStrip,    
 


callback_mode() ->
    handle_event_function.

handle_event(Event, From, Data) ->
        {ok, Data};

handle_event({land_ack, Ans, Plane}, _From, Data) ->
        case Ans of
            true -> {{X1,Y1,Z1},EndStrip} = Plane,
                    UpdatedPlane= Plane#plane{dir=get_dir({X1,Y1,Z1},Data#plane.pos),strip=Data,state=fly_to_strip,endStrip=EndStrip},
                    {next_state, fly_to_strip,UpdatedData};

            false-> NextStateName = flying,
                    Time = Data, 
                    UpdatedPlane = Data#plane{time=Time,state=NextStateName},
                    {next_state, NextStateName, Data}
            end.
                         
handle_event({timeout, Plane}, _FromState, Data) ->
    {X,Y,Z} = Plane#plane.pos,
    Dir = Plane#plane.dir,
    Speed = Plane#plane.speed,
    Xnew = math:floor(X + Speed*math:cos(Dir)),
    Ynew = math:floor(Y + Speed*math:sin(Dir)),
    Time = Plane#plane.time-1,
    gen_server:cast(?MODULE,{update,{Xnew,Ynew,Z},Dir,self()}),        % Send rower my new location
    UpdatedPlane=Plane#plane{pos={Xnew,Ynew,Z}, time=Time},
    {next_state,flying,UpdatedPlane};

handle_event({fly, Plane}, _FromState, Data) ->
        {X,Y,Z} = Plane#plane.pos,
        Dir = Plane#plane.dir,
        Speed = Plane#plane.speed,
        Xnew = math:floor(X + Speed*math:cos(Dir)),
        Ynew = math:floor(Y + Speed*math:sin(Dir)),
        gen_server:cast(?MODULE,{update,{Xnew,Ynew,Z},Dir,self()}),        % Send rower my new location
        UpdatedPlane=Plane#plane{pos={Xnew,Ynew,Z}, time=Time},
        {next_state,fly_to_strip,UpdatedPlane};
    
handle_event({update_takeoff, Plane}, _FromState, Data) ->
    {UpdatedPlane,Speed_Up}= travel_on_strip(Plane,get_dir(Plane#plane.strip),0.5),
    {X,Y,Z}= UpdatedPlane#plane.pos,
    if ((Xend-5 =< X) and (X=<Xend+5)) or ((Yend-5 =< Y) and (Y=<Yend+5)) -> 
            UpPlane = UpdatedPlane#plane.state=flying,
            {NextStateName {next_state,flying,UpPlane};
        true -> 
            UpPlane = UpdatedPlane#plane.state=takeoff, 
            {next_state,takeoff,UpPlane}
    end.



handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%handle function 





get_dir({{X,Y,Z},{X1,Y1,Z1}})->(180 * math:atan(abs(Y1-Y)/abs(X1 - X)))/math:pi().

travel_on_strip(Plane,Teta,Factor)->
    {X,Y,Z} = Plane#plane.pos,
    UpdatedSpeed = Factor + Plane#plane.speedup,
    Xnew = math:floor(X + UpdatedSpeed*math:cos(Teta)),
    Ynew = math:floor(Y + UpdatedSpeed*math:sin(Teta)),
    UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}, speedup= UpdatedSpeed},
    gen_server:cast(?MODULE,{update,{Xnew,Ynew,Z},Teta,self()}),        % Send rower my new location
    UpdatedPlane.
    

travel_to_location(Plane,Teta)->
    {X,Y,Z} = Plane#plane.pos,
    Speed = Plane#plane.speed,
    {PosStart,{Xend,Yend,Zend}} = Plane#plane.strip,
    Xnew = math:floor(X + Speed*math:cos(Teta)),
    Ynew = math:floor(Y + Speed*math:sin(Teta)),
    UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}},
    gen_server:cast(?MODULE,{update,{Xnew,Ynew,Z},Teta,self()}),        % Send rower my new location
    if ((Xend-5 =< Xnew) and (Xnew=<Xend+5)) or ((Yend-5 =< Ynew) and (Ynew=<Yend+5)) -> UpdatedPlane;
        true -> travel_to_location(UpdatedPlane,Teta)
    end.








