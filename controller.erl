-module(controller).
-export([loop/4]).
-behaviour(gen_server).

%% API
-export([ start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {dummy}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{dummy=1}}.

handle_call(stop, _From, State) ->
    {stop, normal, stopped, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


loop(ETS1,ETS2,ETS3,ETS4)->
    {graphics, 'py@127.0.0.1'} ! {ets1,[{123,airplane1,random:uniform(800),random:uniform(800),random:uniform(180)},{456,airplane2,random:uniform(800),random:uniform(800),random:uniform(180)}], [self()]},
    timer:sleep(1000),
    loop(ETS1,ETS2,ETS3,ETS4).

