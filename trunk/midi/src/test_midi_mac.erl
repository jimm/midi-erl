%% Author: jakob
%% Created: 5 mar 2009
%% Description: TODO: Add description to test_midi_mac
-module(test_midi_mac).

%%
%% Include files
%%

-include("midi.hrl").
-include("midi_mac.hrl").

%%
%% Exported Functions
%%
-export([z/0, y/0, y/1, w/1, p/0, p/1, q/1, o/1, o/0, v/1, v/0, oct_off/3]).

-import(midi_mac, [new_au_graph/0, au_graph_add_node/2, au_graph_initialize/1, music_device_midi_event/5,
		   au_graph_open/1, au_graph_start/1, get_au_const/1,
		   au_graph_connect_node_input/5, au_graph_node_info/2, music_device_midi_sys_ex/2]).

-import(midi_mac, [create_client/1, list_destinations/0, create_output_port/2, midi_out/7]).

-define(DEBUG, 1).

-compile(export_all).
-ifdef(DEBUG).
-define(D(T), io:format("~p\n", [{?MODULE, ?LINE, T}])).
-else.
-define(D(T), ok).
-endif.

%%
%% API Functions
%%

x() ->
    {ok, Graph, Synth} = createAuGraph(),
    ?D(Synth),
    ok = au_graph_initialize(Graph),
    Channel = 0,
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_CONTROLLER_CHANGE,
				 Channel, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_PROGRAM_CHANGE, 
				 Channel, 0, 0),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_CONTROLLER_CHANGE,
				 Channel+1, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_PROGRAM_CHANGE, 
				 Channel+1, 5, 0),
    ?D(Graph),
    ok = au_graph_start(Graph),
    ?D(Graph),
    NoteNum = 60,
    OnVelocity = 77,
    OffVelocity = 0,
    lists:foreach(fun(N) ->
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel, NoteNum+N, OnVelocity),
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel+1, NoteNum-N, OnVelocity),
			  timer:sleep(50),
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel, NoteNum+N, OffVelocity),
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel+1, NoteNum-N, OffVelocity)
		  end, lists:seq(0, 30)),
    ok.

z() ->
    z(3.4).

z(Speed) ->
    {ok, Graph, Synth} = createAuGraph(),
    ok = au_graph_initialize(Graph),
    ok = au_graph_start(Graph),
    [_ | [F0]] = midi_file:read_midi_file("/Users/jakob/Desktop/jazz.mid"),
%%     F = transpose(F0, -1),
    F = F0,
    lists:foreach(fun({Delay, {Status, Channel, Note, Velocity}}) when Status=:=on; Status=:=off ->
			  StatusNum = midi:get_status_const(Status),
			  ok = music_device_midi_event(Synth, StatusNum, Channel, Note, Velocity),
			  timer:sleep(trunc(Delay*Speed));
		     ({Delay, {program_change, Channel, P}}) ->
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_PROGRAM_CHANGE, Channel, P, 0),
			  timer:sleep(trunc(Delay*Speed));
		     ({Delay, {controller_change, Channel, Controller, Value}}) ->
			  ControllerNum = midi:get_controller_const(Controller),
			  ok = music_device_midi_event(Synth, ?MIDI_STATUS_CONTROLLER_CHANGE, Channel,
						       ControllerNum, Value),
			  timer:sleep(trunc(Delay*Speed));
%% 		     ({Delay, {sysex, Data}}) ->
%% 			  ok = music_device_midi_sys_ex(Synth, Data),
%% 			  timer:sleep(trunc(Delay*Speed));
		     ({Delay, _E}) ->
			  io:format("E ~p\n", [_E]),
			  timer:sleep(trunc(Delay*Speed));
		     (_) ->
			  ok
		  end, F).
y() ->
    y(1).

y(Num) ->
    Ds = list_destinations(),
    {D, _} = lists:nth(Num+1, Ds),
    {C, _} = create_client("test"),
    {O, _} = create_output_port(C, "output"),
    NoteNum = 60,
    Channel = 0,
    OnVelocity = 50,
    OffVelocity = 0,
    lists:foreach(fun(N) ->
			  ok = midi_out(O, D, 0, ?MIDI_STATUS_ON, Channel, NoteNum+N, OnVelocity),
			  ok = midi_out(O, D, 0, ?MIDI_STATUS_ON, Channel, NoteNum-N, OnVelocity),
			  timer:sleep(50),
			  ok = midi_out(O, D, 0, ?MIDI_STATUS_ON, Channel, NoteNum+N, OffVelocity),
			  ok = midi_out(O, D, 0, ?MIDI_STATUS_ON, Channel, NoteNum-N, OffVelocity)
		  end, lists:seq(0, 30)),
    ok.

w(Num) ->
    Ds = midi_mac:list_outputs(),
    {D, _} = lists:nth(Num+1, Ds),
    {ok, Output} = midi_mac:open_output(D),
    NoteNum = 60,
    Channel = 0,
    OnVelocity = 50,
    OffVelocity = 0,
    lists:foreach(fun(N) ->
			  ok = midi_mac:send(Output, {on, Channel, NoteNum+N, OnVelocity}),
			  ok = midi_mac:send(Output, {on, Channel, NoteNum-N, OnVelocity}),
			  timer:sleep(50),
			  ok = midi_mac:send(Output, {on, Channel, NoteNum+N, OffVelocity}),
			  ok = midi_mac:send(Output, {on, Channel, NoteNum-N, OffVelocity})
		  end, lists:seq(0, 30)),
    ok = midi_mac:close(Output),
    ok.

v() ->
    v(1).

receiver_tester() ->
    receive
	{midi_event, _, timing_clock} ->
%% 	    io:format("c "),
	    ok;
	{midi_event, _, active_sensing} ->
%% 	    io:format("a "),
	    ok;
	quit ->
	    exit(normal);
	T ->
	    io:format("got ~p  \n", [T])
    end,
    receiver_tester().

v(Num) ->
    Ds = midi_mac:list_inputs(),
    {D, _} = lists:nth(Num+1, Ds),
    ReceiverPid = spawn(fun() -> receiver_tester() end),
    timer:sleep(100),
    {ok, Input} = midi_mac:open_input(D, ReceiverPid),
    timer:sleep(10000),
    ReceiverPid ! quit,
    midi_mac:close(Input).

q(Num) ->
    Ds = midi_mac:list_inputs(),
    {D, _} = lists:nth(Num+1, Ds),
    ReceiverPid = spawn(fun() -> receiver_tester() end),
    timer:sleep(100),
    {ok, Input} = midi_mac:open_input(D, ReceiverPid),
    w(Num),
    timer:sleep(10000),
    ReceiverPid ! quit,
    midi_mac:close(Input).



o() ->
    o(1).

oct_off(Ch, Note, Output) ->
    io:format("oct_off ~p\n", [Note]),
    midi_mac:send(Output, {on, Ch, Note, 0}).

oct_on(Ons, Output) ->
    lists:foreach(fun({Ch, Note}) ->
			  Note12 = Note+12,
			  io:format("oct_on ~p\n", [Note12]),
			  midi_mac:send(Output, {on, Ch, Note12, 35}),
			  timer:apply_after(50, ?MODULE, oct_off, [Ch, Note12, Output])
		  end, Ons),
    ok.

oct_loop(Ons, Output) ->
    receive
	{midi_event, _, {on, Ch, Note, 0}} ->
	    io:format("off* ~p\n", [Note]),
	    NewOns = Ons -- [{Ch, Note}],
	    oct_loop(NewOns, Output);
	{midi_event, _, {off, Ch, Note, _Vel}} ->
	    io:format("off ~p\n", [Note]),
	    NewOns = Ons -- [{Ch, Note}],
	    oct_loop(NewOns, Output);
	{midi_event, _, {on, Ch, Note, _Vel}} ->
	    io:format("on ~p\n", [Note]),
	    NewOns = Ons ++ [{Ch, Note}],
	    oct_on([{Ch, Note}], Output),
	    oct_loop(NewOns, Output);
	{midi_event, _, _} ->
	    oct_loop(Ons, Output);
	quit ->
	    exit(normal);
	T ->
	    io:format("other ~p  \n", [T]),
	    oct_loop(Ons, Output)
    end.

o(Num) ->
    Is = midi_mac:list_inputs(),
    {I, _} = lists:nth(Num+1, Is),
    timer:sleep(100),
    Os = midi_mac:list_outputs(),
    {O, _} = lists:nth(Num+1, Os),
    {ok, Output} = midi_mac:open_output(O),
    ReceiverPid = spawn(fun() -> oct_loop([], Output) end),
    {ok, Input} = midi_mac:open_input(I, ReceiverPid),
    io:format("Input, Output, ReceiverPid ~p\n", [[Input, Output, ReceiverPid]]),
    timer:sleep(100000),
    ReceiverPid ! quit,
    ok = midi_mac:close(Input),
    ok = midi_mac:close(Output).




transpose(L, N) when is_list(L) ->
    [trans(I, N) || I <- L].

trans({Delay, {Status, Channel, NoteNum, Velocity}}, N) when Status=:= on; Status=:=off ->
    {Delay, {Status, Channel, NoteNum+N, Velocity}};
trans(T, _N) ->
    T.

p() ->
    p(1).

p(Num) ->
    Ds = midi_mac:list_outputs(),
    {D, _} = lists:nth(Num+1, Ds),
    {ok, O} = midi_mac:open_output(D),
    NoteNum = 60,
    Channel = 0,
    OnVelocity = 50,
    timer:sleep(1000),
    lists:foreach(fun(N) ->
			  ok = midi_mac:send(O, {on, Channel, NoteNum+N, OnVelocity}),
			  ok = midi_mac:send(O, {on, Channel, NoteNum-N, OnVelocity}),
			  timer:sleep(50),
			  ok = midi_mac:send(O, {on, Channel, NoteNum+N, 0}),
			  ok = midi_mac:send(O, {on, Channel, NoteNum-N, 0})
		  end, lists:seq(0, 30)),
    ok.

b_loop(Output, N) ->
    receive
	{midi_event, _, {on, Ch, Note, 0}} ->
	    io:format("off* ~p\n", [Note]),
	    midi_mac:send(Output, {on, Ch, Note+N, 0}),
	    b_loop(Output, N);
	{midi_event, _, {off, Ch, Note, Vel}} ->
	    io:format("off ~p\n", [Note]),
	    midi_mac:send(Output, {off, Ch, Note+N, Vel}),
	    b_loop(Output, N);
	{midi_event, _, {on, Ch, Note, Vel}} ->
	    io:format("on ~p\n", [Note]),
	    midi_mac:send(Output, {on, Ch, Note+N, Vel}),
	    b_loop(Output, N);
	{midi_event, _, _} ->
	    b_loop(Output, N);
	quit ->
	    exit(normal);
	T ->
	    io:format("other ~p  \n", [T]),
	    b_loop(Output, N)
    end.

b(Num, Int) ->
    Is = midi_mac:list_inputs(),
    {I, _} = lists:nth(Num+1, Is),
    timer:sleep(100),
    Os = midi_mac:list_outputs(),
    {O, _} = lists:nth(Num+1, Os),
    {ok, Output} = midi_mac:open_output(O),
    ReceiverPid = spawn(fun() -> b_loop(Output, Int) end),
    {ok, Input} = midi_mac:open_input(I, ReceiverPid),
    io:format("Input, Output, ReceiverPid ~p\n", [[Input, Output, ReceiverPid]]),
    timer:sleep(100000),
    ReceiverPid ! quit,
    ok = midi_mac:close(Input),
    ok = midi_mac:close(Output).

b(Int) ->
    b(1, Int).

b() ->
    b(1, 7).


%%
%% Local Functions
%%

createAuGraph() ->
    {ok, OutGraph} = new_au_graph(),
    ?D(OutGraph),
    CD = #'ComponentDescription'{componentManufacturer = get_au_const(kAudioUnitManufacturer_Apple)},
    CD1 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_MusicDevice),
				    componentSubType = get_au_const(kAudioUnitSubType_DLSSynth)},
    {ok, SynthNode} = au_graph_add_node(OutGraph, CD1),
    ?D(SynthNode),
    CD2 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_Effect),
				    componentSubType = get_au_const(kAudioUnitSubType_PeakLimiter)},
    {ok, LimiterNode} = au_graph_add_node(OutGraph, CD2),
    CD3 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_Output),
				    componentSubType = get_au_const(kAudioUnitSubType_DefaultOutput)},
    {ok, OutNode} = au_graph_add_node(OutGraph, CD3),
    ?D(OutNode),
    ok = au_graph_open(OutGraph),
    ?D(ok),
    ok = au_graph_connect_node_input(OutGraph, SynthNode, 0, LimiterNode, 0),
    ?D(ok),
    ok = au_graph_connect_node_input(OutGraph, LimiterNode, 0, OutNode, 0),
    ?D(ok),
    {ok, _CD, OutSynth} = au_graph_node_info(OutGraph, SynthNode),
    ?D(_CD),
    ?D(OutSynth),
    {ok, OutGraph, OutSynth}.
