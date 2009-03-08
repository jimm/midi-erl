-module(midi_mac).

-include("midi.hrl").

%% Platform-specific
-export([get_port/0, test/0, now/0, au_graph_initialize/1, au_graph_start/1,
	 music_device_midi_event/5, new_au_graph/0, au_graph_open/1, au_graph_add_node/2,
	 au_graph_connect_node_input/5, au_graph_node_info/2, music_device_midi_sys_ex/2,
	 get_au_const/1]).

-export([connect_source/2, create_input_port/2, create_output_port/2, create_client/1,
	 list_destinations/0, list_sources/0, list_midi_devices/0, midi_out/7, send_midi/4]).

%% High-level
-export([list_outputs/0, list_inputs/0, open_output/1, open_input/1, close/1,
	 send/2, open_soft_synth_output/0, set_receiver/2]).

%% -define(DEBUG, 1).

-ifdef(DEBUG).
-compile(export_all).
-define(D(T), io:format("~p\n", [{?MODULE, ?LINE, T}])).
-else.
-define(D(T), ok).
-endif.

-define(DRV_SET_SOUND_BANK, 1).
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
-define(DRV_DISPOSE_INPUT_PORT, 22).
-define(DRV_DISPOSE_OUTPUT_PORT, 23).

get_port() ->
    case whereis(erl_midi) of
	undefined ->
	    case erl_ddll:load(".", "erl_midi") of
		ok -> ok;
		{error, already_loaded} -> ok;
		E -> exit(E)
	    end,
	    Port = open_port({spawn, "erl_midi"}, []),
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

create_output_port(Client, Name) ->
    do_op(?DRV_CREATE_OUTPUT_PORT, {Client, Name}).

create_input_port(Client, Name) ->
    do_op(?DRV_CREATE_INPUT_PORT, {Client, Name}).

connect_source(Port, Source) ->
    do_op(?DRV_CONNECT_SOURCE, {Port, Source}).

%% High-level API
list_outputs() ->
    list_destinations().

list_inputs() ->
    list_sources().

open_output(OutputNum) ->
    ok.

open_input(InputNum) ->
    ok.

close(OpenOrInput) ->
    ok.

send(OutputHandle, MidiEvent) ->
    ok.

open_soft_synth_output() ->
    ok.

set_receiver(InputHandle, Pid) ->
    ok.
