-module(plane).
-behaviour(gen_statem).

-export([stop/0, start_link/0]).
-export([init/1, callback_mode/0, handle_event/4, terminate/3, code_change/4]).
-export([takeoff/2]).
-record(plane,{pos,speed,dir,time,status,strip,tower,endStrip}).           

stop() ->
    gen_statem:stop(?MODULE).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([Status,TowerPid,Strip,Pos,Speed,Dirvec,Time]) ->  % initialize plane when starting the program
    Plane = #plane{pos= Pos, 
                  speed= Speed, 
                  dir= Dirvec, 
                  time= Time, 
                  status= Status, 
                  strip=Strip, 
                  tower= TowerPid,
                  endStrip={0,0,0}},         
    {ok,takeoff, Plane}.            % send to start state/first state
%init(_Args) ->
%    {ok, state, []}.


takeoff(cast,Plane = #plane{}) ->    % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
    NextStateName = flying,
    {next_state, NextStateName, Plane}.

flying()->ok.
%% state_functions | handle_event_function | [_, state_enter].
callback_mode() ->
    handle_event_function.

handle_event(enter, _OldState, _State, _Data) ->
    keep_state_and_data;

handle_event(_EventType, _EventContent, _State, _Data) ->
    keep_state_and_data.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.
