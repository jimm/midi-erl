-module(midi_mac).

-include("midi.hrl").
-include("midi_mac.hrl").

%% Platform-specific
-export([get_port/0, test/0, now/0, au_graph_initialize/1, au_graph_start/1,
	 music_device_midi_event/5, new_au_graph/0, au_graph_open/1, au_graph_add_node/2,
	 au_graph_connect_node_input/5, au_graph_node_info/2, music_device_midi_sys_ex/2,
	 get_au_const/1]).

-export([connect_source/2, create_input_port/2, create_output_port/2, create_client/1,
	 list_destinations/0, list_sources/0, list_midi_devices/0, midi_out/7, send_midi/4]).

%% High-level
-export([list_outputs/0, list_inputs/0, open_output/1, open_input/2, close/1,
	 send/2, open_soft_synth_output/0]).

%% -define(DEBUG, 1).

-ifdef(DEBUG).
-compile(export_all).
-define(D(T), io:format("~p\n", [{?MODULE, ?LINE, T}])).
-else.
-define(D(T), ok).
-endif.

-define(DRV_MUSIC_DEVICE_MIDI_EVENT, 2).
-define(DRV_DEVICE_LIST, 3).
-define(DRV_SOURCE_LIST, 4).
-define(DRV_DESTINATION_LIST, 5).
-define(DRV_CREATE_CLIENT, 6).
-define(DRV_CONNECT_SOURCE, 7).
-define(DRV_CREATE_INPUT_PORT, 8).
-define(DRV_CREATE_OUTPUT_PORT, 9).
-define(DRV_SEND_MIDI, 10).
-define(DRV_NOW, 11).
-define(DRV_TEST, 12).

-define(DRV_AU_GRAPH_INITIALIZE, 13).
-define(DRV_AU_GRAPH_START, 14).
-define(DRV_NEW_AU_GRAPH, 15).
-define(DRV_AU_GRAPH_ADD_NODE, 16).
-define(DRV_AU_GRAPH_OPEN, 17).
-define(DRV_AU_GRAPH_CONNECT_NODE_INPUT, 18).
-define(DRV_AU_GRAPH_NODE_INFO, 19).
-define(DRV_MUSIC_DEVICE_MIDI_SYS_EX, 20).

-define(DRV_DISPOSE_CLIENT, 21).
-define(DRV_DISPOSE_PORT, 23).
-define(DRV_DISPOSE_AU_GRAPH, 24).

-define(DRV_SEND_MIDI_SYSEX, 25).

get_port() ->
    case whereis(erl_midi) of
	undefined ->
	    case erl_ddll:load(".", "erl_midi") of
		ok -> ok;
		{error, already_loaded} -> ok;
		E -> exit(E)
	    end,
	    Port = open_port({spawn, "erl_midi"}, [binary]),
	    register(erl_midi, Port),
	    Port;
	Port ->
	    Port
    end.

do_op(Op) ->
    do_op(Op, []).

do_op(Op, Data) ->
    Port = get_port(),
    ?D(Data),
    BinData = term_to_binary(Data),
    BinResult = erlang:port_control(Port, Op, BinData),
    binary_to_term(BinResult).

test() ->
    do_op(?DRV_TEST).

now() ->
    <<MidiNow:64/integer-unsigned-native>> = do_op(?DRV_NOW),
    MidiNow.

%% create_au_graph() ->
%%     do_op(?DRV_CREATE_AU_GRAPH).

au_graph_start(Graph) ->
    do_op(?DRV_AU_GRAPH_START, Graph).

au_graph_initialize(Graph) ->
    do_op(?DRV_AU_GRAPH_INITIALIZE, Graph).

music_device_midi_event(Synth, MidiMessage, MidiChannel, Param1, Param2) ->
    do_op(?DRV_MUSIC_DEVICE_MIDI_EVENT, {Synth, MidiMessage, MidiChannel, 
				       Param1, Param2}).

music_device_midi_sys_ex(Synth, Data) ->
    do_op(?DRV_MUSIC_DEVICE_MIDI_SYS_EX, {Synth, Data}).

new_au_graph() ->
    do_op(?DRV_NEW_AU_GRAPH).

au_graph_add_node(Graph, ComponentDescription) ->
    do_op(?DRV_AU_GRAPH_ADD_NODE, {Graph, ComponentDescription}).

au_graph_connect_node_input(Graph, SourceNode, OutputNumber, DestNode, InputNumber) ->
    do_op(?DRV_AU_GRAPH_CONNECT_NODE_INPUT, {Graph, SourceNode, OutputNumber, DestNode, InputNumber}).

au_graph_open(Graph) ->
    do_op(?DRV_AU_GRAPH_OPEN, Graph).

au_graph_node_info(Graph, Node) ->
    do_op(?DRV_AU_GRAPH_NODE_INFO, {Graph, Node}).

get_au_const(A) when is_atom(A) ->
    {value, {_, V}} = lists:keysearch(A, 1, audio_unit_type_map()),
    string4_to_uint(V);
get_au_const(I) when is_integer(I) ->
    S = uint_to_string4(I),
    case lists:keysearch(S, 2, audio_unit_type_map()) of
	{value, {V, _}} ->
	    V;
	_ ->
	    S
    end.

string4_to_uint(S) ->
    [A, B, C, D] = S,
    <<I:32/integer-unsigned-big>> = <<A, B, C, D>>,
    I.

uint_to_string4(I) ->
    <<A, B, C, D>> = <<I:32/integer-unsigned-big>>,
    [A, B, C, D].

audio_unit_type_map() ->
    [{kAudioUnitType_Output, "auou"},
     {kAudioUnitSubType_HALOutput, "ahal"},
     {kAudioUnitSubType_DefaultOutput, "def "},
     {kAudioUnitSubType_SystemOutput, "sys "},
     {kAudioUnitSubType_GenericOutput, "genr"},
     
     {kAudioUnitType_MusicDevice, "aumu"},
     {kAudioUnitSubType_DLSSynth, "dls "},
     
     {kAudioUnitType_MusicEffect, "aumf"},
     
     {kAudioUnitType_FormatConverter, "aufc"},
     {kAudioUnitSubType_AUConverter, "conv"},
     {kAudioUnitSubType_Varispeed, "vari"},
     {kAudioUnitSubType_DeferredRenderer, "defr"},
     {kAudioUnitSubType_TimePitch, "tmpt"},
     {kAudioUnitSubType_Splitter, "splt"},
     {kAudioUnitSubType_Merger, "merg"},
     
     {kAudioUnitType_Effect, "aufx"},
     {kAudioUnitSubType_Delay, "dely"},
     {kAudioUnitSubType_LowPassFilter, "lpas"},
     {kAudioUnitSubType_HighPassFilter, "hpas"},
     {kAudioUnitSubType_BandPassFilter, "bpas"},
     {kAudioUnitSubType_HighShelfFilter, "hshf"},
     {kAudioUnitSubType_LowShelfFilter, "lshf"},
     {kAudioUnitSubType_ParametricEQ, "pmeq"},
     {kAudioUnitSubType_GraphicEQ, "greq"},
     {kAudioUnitSubType_PeakLimiter, "lmtr"},
     {kAudioUnitSubType_DynamicsProcessor, "dcmp"},
     {kAudioUnitSubType_MultiBandCompressor, "mcmp"},
     {kAudioUnitSubType_MatrixReverb, "mrev"},
     {kAudioUnitSubType_SampleDelay, "sdly"},
     {kAudioUnitSubType_Pitch, "tmpt"},
     {kAudioUnitSubType_AUFilter, "filt"},
     {kAudioUnitSubType_NetSend, "nsnd"},
     {kAudioUnitSubType_Distortion, "dist"},
     {kAudioUnitSubType_RogerBeep, "rogr"},
     
     {kAudioUnitType_Mixer, "aumx"},
     {kAudioUnitSubType_StereoMixer, "smxr"},
     {kAudioUnitSubType_3DMixer, "3dmx"},
     {kAudioUnitSubType_MatrixMixer, "mxmx"},
     {kAudioUnitSubType_MultiChannelMixer, "mcmx"},
     
     {kAudioUnitType_Panner, "aupn"},
     {kAudioUnitSubType_SphericalHeadPanner, "sphr"},
     {kAudioUnitSubType_VectorPanner, "vbas"},
     {kAudioUnitSubType_SoundFieldPanner, "ambi"},
     {kAudioUnitSubType_HRTFPanner, "hrtf"},
     
     {kAudioUnitType_OfflineEffect, "auol"},
     
     {kAudioUnitType_Generator, "augn"},
     {kAudioUnitSubType_ScheduledSoundPlayer, "sspl"},
     {kAudioUnitSubType_AudioFilePlayer, "afpl"},
     {kAudioUnitSubType_NetReceive, "nrcv"},
     
     {kAudioUnitManufacturer_Apple, "appl"}].

list_midi_devices() ->
    do_op(?DRV_DEVICE_LIST).

list_sources() ->
    do_op(?DRV_SOURCE_LIST).

list_destinations() ->
    do_op(?DRV_DESTINATION_LIST).

create_client(Name) ->
    do_op(?DRV_CREATE_CLIENT, Name).

midi_out(Port, Endpoint, Timestamp, MidiMessage, MidiChannel, Param1, Param2) ->
    Data = <<MidiMessage:4, MidiChannel:4, Param1, Param2>>,
    send_midi(Port, Endpoint, Timestamp, Data).

send_midi(Port, Endpoint, Timestamp, Data) ->
    BinTimestamp = <<Timestamp:64/integer-unsigned-native>>,
    do_op(?DRV_SEND_MIDI, {Port, Endpoint, BinTimestamp, Data}).

send_midi_sysex(Port, Data) ->
    do_op(?DRV_SEND_MIDI_SYSEX, {Port, Data}).

create_output_port(Client, Name) ->
    do_op(?DRV_CREATE_OUTPUT_PORT, {Client, Name}).

create_input_port(Client, Name) ->
    do_op(?DRV_CREATE_INPUT_PORT, {Client, Name}).

connect_source(MidiPort, Source) ->
    do_op(?DRV_CONNECT_SOURCE, {MidiPort, Source}).

dispose_port(MidiPort) ->
    do_op(?DRV_DISPOSE_PORT, MidiPort).

dispose_client(Client) ->
    do_op(?DRV_DISPOSE_CLIENT, Client).

dispose_au_graph(Graph) ->
    do_op(?DRV_DISPOSE_AU_GRAPH, Graph).

%% High-level API
list_outputs() ->
    list_destinations().

list_inputs() ->
    list_sources().

open_output(Output) ->
    S = integer_to_list(random:uniform(10000)),
    Client = create_client("client"++S),
    Port = create_output_port(Client, "output"++S),
    {ok, {output, Port, Output, Client}}.


open_input(Input, ReceiverPid) ->
    S = integer_to_list(random:uniform(10000)),
    Client = create_client("client"++S),
    Port = create_input_port(Client, "input"++S),
    ok = connect_source(Port, Input),
    ServerPid = spawn_link(fun() -> receive_server(ReceiverPid) end),
    {ok, {input, Port, Client, ServerPid}}.

close(OutputInputOrSoftSynth) ->
    case OutputInputOrSoftSynth of
	{soft, {'AUGraph', _} = Graph, _Synth} ->
	    dispose_au_graph(Graph);
	{output, Port, _Endpoint, Client} ->
	    dispose_port(Port),
	    dispose_client(Client);
	{input, Port, Client, ServerPid} ->
	    ServerPid ! quit,
	    dispose_port(Port),
	    dispose_client(Client)
    end.

send(OutputHandle, MidiEvent) ->
    case OutputHandle of
	{soft, {'AUGraph', _}, Synth} -> 
	    case midi:midi_event_to_numbers(MidiEvent) of
		{MidiMessage, MidiChannel, Param1, Param2} ->
		    music_device_midi_event(Synth, MidiMessage, 
					    MidiChannel, Param1, Param2);
		{_Sysex, Data} ->
		    music_device_midi_sys_ex(Synth, Data)
	    end;
	{output, Port, Endpoint, _Client} ->
	    case midi:midi_event_to_numbers(MidiEvent) of
		{MidiMessage, MidiChannel, Param1, Param2} ->
		    midi_out(Port, Endpoint, 0, MidiMessage, 
			     MidiChannel, Param1, Param2);
		{_Sysex, Data} ->
		    send_midi_sysex(Port, Data)
	    end
    end.

open_soft_synth_output() ->
    {ok, OutGraph} = new_au_graph(),
    CD = #'ComponentDescription'{componentManufacturer = get_au_const(kAudioUnitManufacturer_Apple)},
    CD1 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_MusicDevice),
				    componentSubType = get_au_const(kAudioUnitSubType_DLSSynth)},
    {ok, SynthNode} = au_graph_add_node(OutGraph, CD1),
    CD2 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_Effect),
				    componentSubType = get_au_const(kAudioUnitSubType_PeakLimiter)},
    {ok, LimiterNode} = au_graph_add_node(OutGraph, CD2),
    CD3 = CD#'ComponentDescription'{componentType = get_au_const(kAudioUnitType_Output),
				    componentSubType = get_au_const(kAudioUnitSubType_DefaultOutput)},
    {ok, OutNode} = au_graph_add_node(OutGraph, CD3),
    ok = au_graph_open(OutGraph),
    ok = au_graph_connect_node_input(OutGraph, SynthNode, 0, LimiterNode, 0),
    ok = au_graph_connect_node_input(OutGraph, LimiterNode, 0, OutNode, 0),
    {ok, _CD, OutSynth} = au_graph_node_info(OutGraph, SynthNode),
    {ok, {soft, OutGraph, OutSynth}}.

receive_server(Pid) ->
    erlang:port_connect(get_port(), Pid),
    receive_server_loop(Pid).

receive_server_loop(Pid) ->
    receive
	{_Port, {MidiData}} ->
	    <<MidiNow:64/integer-unsigned-native, Data>> = MidiData,
	    Event = midi:get_event(Data),
	    Pid ! {midi_event, MidiNow, Event},
	    receive_server_loop(Pid);
	quit ->
	    ok;
	Other ->
	    Pid ! {other, Other},
	    receive_server_loop(Pid)
    end.




