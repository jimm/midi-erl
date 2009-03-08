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
-export([z/0]).

-import(midi_mac, [new_au_graph/0, au_graph_add_node/2, au_graph_initialize/1, music_device_midi_event/5,
		   au_graph_open/1, au_graph_start/1, create_au_graph/0, get_au_const/1,
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

t() ->
    {ok, Graph, Synth} = create_au_graph(),
    ?D(Synth),
    ok = au_graph_initialize(Graph),
    Channel = 0,
    ?D(Graph),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_CONTROLLER_CHANGE,
			       Channel, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ?D(Graph),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_PROGRAM_CHANGE, 
			       Channel, 10, 0),
    ?D(Graph),
    ok = au_graph_start(Graph),
    ?D(Graph),
    NoteNum = 60,
    OnVelocity = 127,
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel, NoteNum, OnVelocity),
    ?D(Graph),
    timer:sleep(1000),
    ok = music_device_midi_event(Synth, ?MIDI_STATUS_ON, Channel, NoteNum, 0),
    ?D(Graph),
    ok.

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
    {C, _} = create_client("test"),
    [{D, _} | _] = list_destinations(),
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

transpose(L, N) when is_list(L) ->
    [trans(I, N) || I <- L].

trans({Delay, {Status, Channel, NoteNum, Velocity}}, N) when Status=:= on; Status=:=off ->
    {Delay, {Status, Channel, NoteNum+N, Velocity}};
trans(T, _N) ->
    T.

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
