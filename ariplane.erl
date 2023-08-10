%%%------------------------------------------------------------------------------------------------------------------------
%%% project
%%% @The creator of the project: 
%%% @doc
%%% 
%%% @end
%%% Created : 
%%%-------------------------------------------------------------------------------------------------------------------------

-module(ariplane).
-author("ShahafZohar, Danny Blozorov").
-behavior(gen_server).
-import(rand, [uniform/0]).

%% API
-export([start_link/0,start/5,start/8]).
%% gen_statem callbacks
-export([
  init/1,
  format_status/2,
  state_name/3,
  handle_event/4,
  terminate/3,
  code_change/4,
  callback_mode/0
]).
          
-record(plane,{pos,speed,dir,time,status,type,strip,monitor,tower}).           

-define(SERVER,?MODULE).


%%--------------------------------------------------------------------------------------------------------
%% @doc
%% Creates a gen_statem process which is calls Module:init/1 to initialize.
%% To ensure a syncrhronized start-up prodedure, this function does not 
%% return until Module:init/1 has returned.
%%
%% @spec start _link() -> {ok,Pid} | ignore {error, Error}
%% @end
%%--------------------------------------------------------------------------------------------------------

start_link() -> gen_statem:start_link({local,?SERVER},?MODULE,[],[]).
start(Name,AirplaneMonitor,Type,Start,PC) ->
  gen_statem:start({local, Name}, ?MODULE, [Name,CarMonitor,Start,Type,PC], []).
start(Name,PlaneMonitor,Type,Start,Location,Strip,PC) -> gen_statem:start({local, Name}, ?MODULE, [Name,PlaneMonitor,Type,Start,Location,Strip,PC], []).

%%%===================================================================
%%% gen_statem callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_statem is started using gen_statem:start/[3,4] or
%% gen_statem:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {CallbackMode, StateName, State} |
%%                     {CallbackMode, StateName, State, Actions} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-record(plane,{pos,speed,dir,time,status,type,strip,monitor,tower}).           

init([PlaneMonitor,Status,Tower,Type,Strip,Pos,Speed,Dirvec,Time]) ->  % initialize plane when starting the program
                               %% plane monitor, initial location and speed in process dictionary
            put(planeMon,PlaneMonitor),
            put(time, Time),                   % Time in the air
            put(position, Pos),                % Location of the plane (x,y,z)
            put(status,Status),                % the status can be : {flying, landing, taking off}
            put(type ,Type),                   % Type plane 
            put(speed ,Type),                  % speed as scalar
            put(strip ,Strip),                 % strip : ({Xstart,Ystart,Zstart},{Xend, Yend, Zend}, pid)
            put(tower, Tower),                 % Each plane is under a different computer's (Tower) area   
            put(dir,Dirvec),      
            Plane = #plane{pos= Pos, speed= Speed, dir= Dirvec, time= Time, status= Status, type= Type, strip=Strip, monitor= PlaneMonitor, tower= Tower}         
            ets:insert(planes,{self(),Plane}),       % insert the plane to the planes ets
            PlaneMonitor ! {add_to_monitor,self()},                               % add the car to the monitor
            {ok,start_state, Plane}. % send to start state/first state

            case Con of         % check in which state the car was and send it to the same state
              {drive_straight} -> {ok,drive_straight,#cars_state{},Type};
              {idle} ->{ok,idle, #cars_state{},Type} ;
              {turning,C,Dir,Road} ->{ok,turning, #cars_state{turnCounter = C, nextTurnDir = Dir, nextTurnRoad = Road},Type} ;
              {bypassing,C} -> {ok,bypassing, #cars_state{bypassCounter = C},Type}
          
          
            end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_statem when it needs to find out
%% the callback mode of the callback module.
%%
%% @spec callback_mode() -> atom().
%% @end
%%--------------------------------------------------------------------

callback_mode() ->
  state_functions.

%% Events
flying(Pid) -> gen_statem:cast(Pid,{fly,Pid}). % plane is now flying in some area, send location to tower. 
requestLanding(Pid,Tower) -> gen_statem:cast(Pid,{reqland,Pid,Tower}). % Plan what to leand, send request to tower near by
gotLandingStrip(Pid,Pos)->gen_statem:cast(Pid,{landing,Pos}) % plane got Strip location and need to land there 
turns(Pid,Dir) -> gen_statem:cast(Pid,{turn,Pid,Dir}). % plane turns back after rich the wall
switchTower(Pid,From,To)->gen_statem:cast(Pid,{switch,Pid,From,To}). % now the plane need to die and create from the ithaer tower.
kill(Pid) ->  gen_statem:cast(Pid,{kill,Pid}).         % kill plan



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Called (1) whenever sys:get_status/1,2 is called by gen_statem or
%% (2) when gen_statem terminates abnormally.
%% This callback is optional.
%%
%% @spec format_status(Opt, [PDict, StateName, State]) -> term()
%% @end
%%--------------------------------------------------------------------

format_status(_Opt, [_PDict, _StateName, _State]) ->
                            Status = some_term,
                            Status.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% There should be one instance of this function for each possible
%% state name.  If callback_mode is statefunctions, one of these
%% functions is called when gen_statem receives and event from
%% call/2, cast/2, or as a normal process message.
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
%%--------------------------------------------------------------------


state_name(_EventType, _EventContent, State) ->
        NextStateName = next_state,
        {next_state, NextStateName, State}.

start_state(State = #plabe{}) -> % Start state of the plane, spawn new procsse (plane) and add dictionary and monitor
  PlanePid = spawn(flying,[self()]),
  Monitor = State#plane.monitor,
  Monitor ! {add_to_monitor,SensorPid},
  NextStateName = flying_straight,
  {next_state, NextStateName, State,get(speed)}.

flying_straight(cast,{send,From,Msg},State = #cars_state{})-> % send message to communication tower or to a different car
  {Bool1,To} = check_comms_d(Who,ets:first(comms)),
  case Bool1 of % if there is a close comm tower, send the message to it, else, send to a close car
    true -> communication_tower:receive_message(To,From,Msg);
    _-> {Bool2,To2} = check_close_car(Who,ets:first(cars),From),
      case Bool2 of
        true -> cars:send_msg(To2,{From,Msg});

        _-> [{_,[To3]}] = ets:lookup(comms,ets:first(comms)),
          communication_tower:receive_message(To3,From,Msg)
      end
  end,


flydirctly(cast,{send,From},State=#plane) ->  













