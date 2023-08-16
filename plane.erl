%%%------------------------------------------------------------------------------------------------------------------------
%%% project
%%% @The creator of the project: 
%%% @doc
%%% 
%%% @end
%%% Created : 16.8.23 3:43 AM
%%%-------------------------------------------------------------------------------------------------------------------------

-module(plane).
-behaviour(gen_statem).
-author("ShahafZohar, Danny_Blozorov, Dean_elimelech").

%%API
-export([stop/0, start_link/1]).
-export([init/1, state_name/3,callback_mode/0, terminate/3, code_change/4]).

% States:
-export([takeoff/3,flying/3,landing_request/3]).
-record(plane,{pos,speed,dir,time,state,strip,tower,endStrip,speedup}).           


stop() ->
    gen_statem:stop(?MODULE).
%%--------------------------------------------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which is calls Module:init/1 to initialize.
%% To ensure a syncrhronized start-up prodedure, this function does not 
%% return until Module:init/1 has returned.
%%
%% @spec start _link() -> {ok,Pid} | ignore {error, Error}
%% @end
%%--------------------------------------------------------------------------------------------------------
start_link([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->
    gen_statem:start({local, ?MODULE}, ?MODULE, [Status,TowerPid,Strip,Pos,Speed,Dirvec,Time], []).

%%------------------------------------------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
%% @spec callback_mode() -> atom().
%% @end
%%---------------------------------------------------------------------------------------------------
callback_mode() -> state_functions.    

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
    {ok,Status, Plane}.                         % send to start state/first state

%%------------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these functions is called when gen_statem
%% receives and event from call/2, cast/2, or as a normal process message.
%%
%% @spec state_name(Event, From, State) ->
%%                   {next_state, NextStateName, NextState} |
%%                   {next_state, NextStateName, NextState, Actions} |
%%                   {stop, Reason, NewState} |
%%    				 stop |
%%                   {stop, Reason :: term()} |
%%                   {stop, Reason :: term(), NewData :: data()} |
%%                   {stop_and_reply, Reason, Replies} |
%%                   {stop_and_reply, Reason, Replies, NewState} |
%%                   {keep_state, NewData :: data()} |
%%                   {keep_state, NewState, Actions} |
%%                   keep_state_and_data |
%%                   {keep_state_and_data, Actions}
%% @end
%%------------------------------------------------------------------------------------

state_name(_EventType, _EventContent, State) ->
  NextStateName = next_state,
  {next_state, NextStateName, State}.

%-------------------------------------------------------------------------------------------------------------------------

takeoff(info,{State},Plane = #plane{}) ->        % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
    io:format("~n=========takeoff========~n"),
    {_Start,{Xend,Yend,Zend}} = Plane#plane.strip,
    %io:format("~n~p~n",[get_dir(Plane#plane.strip)]),
    UpdatedPlane= travel(Plane, get_dir(Plane#plane.strip)), {X,Y,_Z}= UpdatedPlane#plane.pos,
    io:format("~n~p~n",[UpdatedPlane]),
    if (Xend < X) and (Yend < Y) -> POS={Xend,Yend,Zend}, NextState =flying;
       true -> POS= UpdatedPlane#plane.pos, NextState=State
       end,
    UpPlane = UpdatedPlane#plane{pos= POS, state=NextState},
    io:format("~n========================~n"),
    erlang:send_after(50, self(), {NextState}),
    {next_state,NextState,UpPlane}.

%-------------------------------------------------------------------------------------------------------------------------

flying(info,{State}, Plane = #plane{}) ->               % send message to communication tower
    io:format("~n=========flying========~n"),
    {X,Y,Z} = Plane#plane.pos,
    UpdatedPlane= travel(Plane,Plane#plane.dir),
    Time = UpdatedPlane#plane.time-1,
    if Time == 0 -> NextState =landing_request;
       true -> NextState = State
    end,
    UpPlane=UpdatedPlane#plane{time=Time,state=NextState},
    erlang:send_after(50, self(), {NextState}),
    {next_state, NextState, UpPlane};

flying(cast,{land_ack, Ans, Data}, Plane = #plane{}) ->               % send message to communication tower
    io:format("~n=========flying========~n"),
    case Ans of 
        yes -> {X,Y,Z} = Plane#plane.pos,
            {{X1,Y1,Z1},EndStrip} = Data,
            Teta = get_dir({{X1,Y1,Z1},{X,Y,Z}}),
            UpdatedPlane= Plane#plane{dir=Teta,strip=Data, state=fly_to_strip, endStrip=EndStrip};
        no -> UpdatedPlane = Plane#plane{time=Data,state=flying}
    end,
    erlang:send_after(50, self(), {Plane#plane.state}),
    {next_state, Plane#plane.state, UpdatedPlane}.

%-------------------------------------------------------------------------------------------------------------------------

landing_request(info,{_State},Plane = #plane{})->                      % send message to communication tower
    io:format("~n=========landing request========~n"),
    gen_server:cast(Plane#plane.tower,{land_req, Plane#plane.pos, self()}),        % Send tower landing requst
    UpdatedPlane = Plane#plane{time=5,state=landing_request},           % add time to wait untill get message landing 
    erlang:send_after(100, self(), {flying}),
    {next_state, flying, UpdatedPlane}.

%-------------------------------------------------------------------------------------------------------------------------
fly_to_strip(info,{State}, Plane = #plane{})->
    io:format("~n============fly_to_strip================~n"),
    {{XStart,YStart,Zstart},{Xend,Yend,_Zend}}=Plane#plane.strip, 
    {X,Y,_Z}=Plane#plane.pos,
    Teta = 180*math:atan(abs(Yend-Y)/abs(Xend-X))/math:pi(),
    io:format("~n============Teta================~n"),
    UpdatedPlane = travel(Plane,Teta),
    {Xnew,Ynew,Znew}=UpdatedPlane#plane.pos,
    if ((Xend < Xnew) or (Yend < Ynew)) ->
            NewTeta = 180*math:atan(abs(Yend-YStart)/abs(Xend-XStart))/math:pi(),
            erlang:send_after(100, self(), {landing}),
            {next_state, landing, Plane#plane{dir=NewTeta,state=landing}};
    true->  erlang:send_after(100, self(), {fly_to_strip}),
        {next_state, fly_to_strip, UpdatedPlane}
    end;

fly_to_strip(cast,{die,State}, Plane = #plane{}) -> ok.

%-------------------------------------------------------------------------------------------------------------------------
%landing(info,{State},Plane=#plane)->ok.


%---------------------------------------------------------------------------------------------------------------------------


% handle_event({land_ack, Ans, Plane}, _From, Data) ->
%     case Ans of
%         true -> {{X1,Y1,_Z1},EndStrip} = Plane,
%             {X,Y,_Z}=Data#plane.pos,
%             Teta = 180*math:atan(abs(Y1-Y)/abs(X1-X))/math:pi(),
%             UpdatedPlane= Plane#plane{dir=Teta,strip=Data,state=fly_to_strip,endStrip=EndStrip},
%             {next_state, fly_to_strip,UpdatedPlane};

%         false-> NextStateName = flying,
%             Time = Data, 
%             UpdatedPlane = Data#plane{time=Time,state=NextStateName},
%             {next_state, NextStateName, Data}
%         end.

terminate(_Reason, _State, _Data) ->
    io:format("Terminate reason = ~p",[_Reason]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

get_dir({{X,Y,_Z},{X1,Y1,_Z1}}) -> math:atan((Y1-Y)/(X1-X)). 

%Rad to degree
convert(Teta)-> 180*Teta/math:pi().

%Teta in rad
travel(Plane,Teta)->
        {X,Y,Z} = Plane#plane.pos,
        Xnew = 1+X+trunc(Plane#plane.speed*math:cos(Teta)),
        Ynew = 1+Y+trunc(Plane#plane.speed*math:sin(Teta)),
        UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}},
        io:format("~n~p~n",[UpdatedPlane]),
        if UpdatedPlane#plane.state == takeoff -> gen_server:cast(Plane#plane.tower,{update,{Xnew,Ynew,Z},convert(Teta),self()});        % Send rower my new location
           true -> 
               gen_server:cast(Plane#plane.tower,{update,{Xnew,Ynew,Z},convert(Teta),self()})
        end,
        UpdatedPlane.
