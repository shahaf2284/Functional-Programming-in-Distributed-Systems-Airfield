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
-export([takeoff/3,flying/3,landing_request/3,fly_to_strip/3, landing/3,landed/3]).
-record(plane,{pos,speed,dir,time,state,strip,tower}).           


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
    gen_statem:start(?MODULE, [Status,TowerPid,Strip,Pos,Speed,Dirvec,Time], []).

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
    %io:format("~n=========init==========~n"),
    erlang:process_flag(trap_exit,true) ,   
    Plane = #plane{pos= Pos, 
                  speed= Speed, 
                  dir= Dirvec, 
                  time= Time, 
                  state= Status, 
                  strip=Strip, 
                  tower= TowerPid}, 
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
    %io:format("~n=========takeoff========~n"),
    {_Start,{Xend,Yend,Zend}} = Plane#plane.strip,
    UpdatedPlane= travel(Plane, get_dir(Plane#plane.strip)), 
    {X,Y,_Z}= UpdatedPlane#plane.pos,
    if (Xend < X) andalso (Yend < Y) -> POS={Xend,Yend,Zend}, NextState =flying;
       true -> POS= UpdatedPlane#plane.pos, NextState=State
       end,
    UpPlane = UpdatedPlane#plane{pos= POS, state=NextState},
    %io:format("~n========================~n"),
    erlang:send_after(50, self(), {NextState}),
    {next_state,NextState,UpPlane}.

%-------------------------------------------------------------------------------------------------------------------------

flying(info,{State}, Plane = #plane{}) ->               % send message to communication tower
    %io:format("~n=========flying========~n"),
    Tetadeg=degrees_to_radians(Plane#plane.dir),
    UpdatedPlane= travel(Plane,Tetadeg),
    Temp_Time = UpdatedPlane#plane.time,
    Time = Temp_Time-1,
    if Time =:= 1 -> NextState =landing_request;
       true -> NextState = State
    end,
    UpPlane=UpdatedPlane#plane{time=Time,state=NextState},
    erlang:send_after(500, self(), {NextState}),
    {next_state, NextState, UpPlane};


flying(cast,{land_ack, Ans,Data},Plane) ->
    case Ans of 
        yes ->
            %io:format("~n=========flying->fly to strip========~n"), 
            {Startstrip,EndStrip} = Plane#plane.strip,
            Teta = get_dir({EndStrip,Plane#plane.pos}),
            UpdatedPlane= Plane#plane{dir=Teta, state=fly_to_strip,time=8};
        no ->
            %io:format("~n=========flying->keep flying========~n"),  
            UpdatedPlane = Plane#plane{time=8,state=flying}
    end,
    %io:format("~n=========State in ~p========~n",[UpdatedPlane#plane.state]), 
    erlang:send_after(500, self(), {UpdatedPlane#plane.state}),
    {next_state, UpdatedPlane#plane.state, UpdatedPlane};

% when plane near wall tower send me to spin 
flying(cast,{spin,Theta},Plane) ->
    UpdatedPlane = Plane#plane{dir=Theta},
    erlang:send_after(1, self(), {flying}),
    {keep_state, UpdatedPlane};

flying(cast,{plane_landed},Plane) ->
    erlang:send_after(1, self(), {landed}),
    {next_state,landed ,Plane}.
%-------------------------------------------------------------------------------------------------------------------------

landing_request(info,{_State},Plane = #plane{})->                      % send message to communication tower
    gen_server:cast(Plane#plane.tower,{land_req, self()}),        % Send tower landing requst
    UpdatedPlane = Plane#plane{time=100,state=landing_request},           % add time to wait untill get message landing 
    erlang:send_after(1, self(), {flying}),
    %io:format("~n=========landing request========~n"),
    {next_state, flying, UpdatedPlane};

landing_request(cast,{spin,Theta},Plane) ->
    UpdatedPlane = Plane#plane{dir=Theta},
    erlang:send_after(1, self(), {flying}),
    {next_state, flying, UpdatedPlane}.
%-------------------------------------------------------------------------------------------------------------------------
fly_to_strip(info,{State}, Plane = #plane{})->
    %io:format("~n============fly to strip================~n"),
    %io:format("~n============~p================~n",[Plane#plane.time]),
    {_Varible,{Xend,Yend,Zend}}=Plane#plane.strip,
    UpdatedPlane= travel(Plane, get_dir({Plane#plane.pos,{Xend,Yend,Zend}})),
    Time= UpdatedPlane#plane.time-1,
    if Time == 1 -> 
            NextState=landing;
        true-> NextState=fly_to_strip
    end,
    UpPlane = UpdatedPlane#plane{time=Time ,state=NextState},
    erlang:send_after(500, self(), {NextState}),
    {next_state, NextState, UpPlane};
% when plane near wall tower send me to spin 
% 
fly_to_strip(cast,{spin,Theta},Plane) ->
    erlang:send_after(1, Plane#plane.tower, {landed,self()}),
    {next_state,landed ,Plane};

fly_to_strip(cast,{plane_landed},Plane) ->
    erlang:send_after(1, self(), {landed}),
    {next_state,landed ,Plane}.
%-------------------------------------------------------------------------------------------------------------------------
landing(info,{State},Plane=#plane{}) -> 
    %io:format("~n**************Landing**********************~n"),
    erlang:send_after(1, Plane#plane.tower, {landed,self()}),
    {next_state,landed,Plane};

landing(cast,{spin,Theta},Plane) ->
    erlang:send_after(1, Plane#plane.tower, {landed,self()}),
    {next_state,landed ,Plane};

landing(cast,{plane_landed},Plane) ->
        erlang:send_after(1, self(), {landed}),
        {next_state,landed ,Plane}.

%---------------------------------------------------------------------------------s------------------------------------------

landed(info,{State},Plane=#plane{}) -> 
    gen_server:cast(Plane#plane.tower,{landed,self()}),
    %io:format("~n**************landed**********************~n"),
    {next_state,State,Plane};

landed(cast,{plane_landed},Plane=#plane{}) -> 
        %io:format("~n****************plane die********************~n"),
        exit(normal).

terminate(_Reason, _State, _Data) ->
    io:format("Terminate reason = ~p",[_Reason]),
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

get_dir({{X,Y,_Z},{X1,Y1,_Z1}}) ->
        if abs(X1-X)<0.03 -> Return = 1.570796327;
            true -> Return = math:atan((Y1-Y)/(X1-X))
        end,
        Return.

convert_rad_to_deg(Radians) ->
    Degrees = Radians * 180 / math:pi(),
    Degrees.

degrees_to_radians(Degrees) ->
    Radians = Degrees * math:pi()/ 180,
    Radians.

travel(Plane,Teta)->
    %io:format("~n--------------Teta= ~p-------------------~n",[Teta]),
    {X,Y,Z} = Plane#plane.pos,
    Xnew = X+Plane#plane.speed*math:cos(Teta),
    Ynew = Y+Plane#plane.speed*math:sin(Teta),
    UpdatedPlane = Plane#plane{pos={Xnew,Ynew,Z}},
    gen_server:cast(Plane#plane.tower,{update,{Xnew,Ynew,Z},convert_rad_to_deg(Teta),self()}),       % Send rower my new location
    UpdatedPlane.
