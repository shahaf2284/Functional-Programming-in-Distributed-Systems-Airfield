-module(plane).
-behaviour(gen_statem).

-export([stop/0, start_link/0]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-export([takeoff/2]).
-record(plane,{pos,speed,dir,time,state,strip,tower,endStrip}).           
-define(SERVER,?MODULE).

stop() ->
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->  % initialize plane when starting the program
    Plane = #plane{pos= Pos, 
                  speed= Speed, 
                  dir= Dirvec, 
                  time= Time, 
                  state= Status, 
                  strip=Strip, 
                  tower= TowerPid,
                  endStrip={0,0,0}},         
    {ok,Status, Plane}.            % send to start state/first state
%init(_Args) ->
%    {ok, state, []}.


takeoff({start},Plane = #plane{}) ->    % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
    UpdatedPlane = Plane#plane{state = flying},
    {next_state, flying, UpdatedPlane}.

flying({fly},Plane = #plane{})->      % send message to communication tower
    {X,Y,Z} = Plane#plane.pos,
    Dir = Plane#plane.dir,
    Speed = Plane#plane.speed,
    Xnew = math:floor(X + Speed*math:cos(Dir)),
    Ynew = math:floor(Y + Speed*math:sin(Dir)),
    Time = Plane#plane.time-1,
    UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}, time=Time},
    gen_server:cast(?MODULE,{update,{Xnew,Ynew,Z},Dir,self()}),        % Send rower my new location
    case Time of
        0 -> NextStateName = landing_request;
        _Any -> NextStateName = flying
    end, 
    {next_state, NextStateName, UpdatedPlane}.

landing_request({request},Plane = #plane{})-> % send message to communication tower
    gen_server:call(?MODULE,{landing_request,Plane#plane.pos,self()}),   % Send rower my new location
    UpdatedPlane = Plane#plane{time=1000,state=landing_request},      %add time to wait untill get message landing 
    {next_state, flying, UpdatedPlane}.

callback_mode() ->
    handle_event_function.

handle_event(Event, From, Data) ->
        {ok, Data};

handle_event({land_ack, Ans, Data}, _From, Data) ->
        case Ans of
            true -> {X,Y,Z} = Data#plane.pos,
                    {{X1,Y1,Z1},EndStrip} = Data,
                    Teta=(180* math:atan(abs(Y1-Y)/abs(X1 - X)))/math:pi(),
                    UpdatedData= Data#plane{dir=Teta,strip={X1,Y1,Z1},state=fly_to_strip,endStrip=EndStrip},
                    {next_state, fly_to_strip,UpdatedData};
                      
            false-> NextStateName = flying,
                    Time = Data, 
                    UpdatedPlane = Data#plane{time=Time,state=NextStateName},
                    {next_state, NextStateName, Data}
            end.
                    
handle_event(send_myself, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
