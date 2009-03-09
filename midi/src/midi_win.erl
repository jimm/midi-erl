-module(midi_win).

-include("midi.hrl").

-export([open_midi_output/1, open_midi_input/1, list_midi_inputs/0, list_midi_outputs/0, midi_out/5,
	 send_midi/2, close_midi_input/1, close_midi_output/1, test/0, now/0]).

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

-define(DRV_LIST_MIDI_INPUTS, 1).
-define(DRV_LIST_MIDI_OUTPUTS, 2).
-define(DRV_OPEN_MIDI_INPUT, 3).
-define(DRV_OPEN_MIDI_OUTPUT, 4).
-define(DRV_SEND_MIDI, 5).
-define(DRV_NOW, 6).
-define(DRV_TEST, 7).
-define(DRV_CLOSE_MIDI_INPUT, 8).
-define(DRV_CLOSE_MIDI_OUTPUT, 9).

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
    do_op(?DRV_NOW).

list_midi_inputs() ->
    do_op(?DRV_LIST_MIDI_INPUTS).

list_midi_outputs() ->
    do_op(?DRV_LIST_MIDI_OUTPUTS).

midi_out(ID, MidiMessage, MidiChannel, Param1, Param2) ->
    <<Msg:32/integer-native-unsigned>> = 
	<<MidiMessage:4, MidiChannel:4, Param1, Param2, 0>>,
    do_op(?DRV_SEND_MIDI, {ID, Msg}).

send_midi(ID, Data) ->
    do_op(?DRV_SEND_MIDI, {ID, Data}).

open_midi_output(ID) ->
    do_op(?DRV_OPEN_MIDI_OUTPUT, ID).

open_midi_input(ID) ->
    do_op(?DRV_OPEN_MIDI_INPUT, ID).

close_midi_input(ID) ->
    do_op(?DRV_CLOSE_MIDI_INPUT, ID).

close_midi_output(ID) ->
    do_op(?DRV_CLOSE_MIDI_OUTPUT, ID).


%% High-level
close(ID) ->
    case ID of
	{{midiin, _}=Port, ServerPid} ->
	    ServerPid ! quit,
	    close_midi_input(Port);
	{midiout, _} ->
	    close_midi_output(ID);
	_ ->
	    {error, bad_midi_port}
    end.

%% High-level API
list_outputs() ->
    list_midi_outputs().

list_inputs() ->
    list_midi_inputs().

open_output(Output) ->
    {ok, open_midi_output(Output)}.

open_input(Input, ReceiverPid) ->
    Port = open_midi_output(Input),
    ServerPid = spawn_link(fun() -> receive_server(ReceiverPid) end),
    {ok, {Port, ServerPid}}.

send(OutputHandle, MidiEvent) ->
    case midi:midi_event_to_numbers(MidiEvent) of
	{MidiMessage, MidiChannel, Param1, Param2} ->
	    midi_out(OutputHandle, MidiMessage, 
		     MidiChannel, Param1, Param2);
	{_Sysex, Data} ->
	    send_midi(OutputHandle, Data)
    end.

open_soft_synth_output() ->
    {ok, Outputs} = list_midi_outputs(),
    {value, {N, _}} = lists:keysearch("Microsoft GS Wavetable SW Synth", 2, Outputs),
    open_output(N).

receive_server(Pid) ->
    erlang:port_connect(get_port(), Pid),
    receive_server_loop(Pid).

receive_server_loop(Pid) ->
    receive
	{_Port, {MidiData}} ->
	    <<MidiNow:32/integer-unsigned-native, Data>> = MidiData,
	    Event = midi:get_event(Data),
	    Pid ! {midi_event, MidiNow, Event},
	    receive_server_loop(Pid);
	quit ->
	    ok;
	Other ->
	    Pid ! {other, Other},
	    receive_server_loop(Pid)
    end.

