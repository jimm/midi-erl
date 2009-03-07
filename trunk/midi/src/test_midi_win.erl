%% Author: jakob
%% Created: 5 mar 2009
%% Description: TODO: Add description to test_midi_mac
-module(test_midi_win).

% cd("c:/documents and settings/jakob/workspace/midi/Debug_win"), code:add_pathz("c:/documents and settings/jakob/workspace/midi/ebin").     

%%
%% Include files
%%

-include("midi.hrl").

%%
%% Exported Functions
%%
-export([z/0]).

-import(midi_win, [open_midi_output/1, open_midi_input/1, midi_out/5]).

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
    ID = open_midi_output(0),
    ?D(ID),
    Channel = 0,
    ok = midi_out(ID, ?MIDI_STATUS_CONTROLLER_CHANGE,
		  Channel, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ?D(ID),
    ok = midi_out(ID, ?MIDI_STATUS_PROGRAM_CHANGE, 
		  Channel, 10, 0),
    ?D(ID),
    NoteNum = 60,
    OnVelocity = 127,
    ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum, OnVelocity),
    ?D(ID),
    timer:sleep(1000),
    ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum, 0),
    ok.

x() ->
    ID = open_midi_output(0),
    ?D(ID),
    Channel = 0,
    ok = midi_out(ID, ?MIDI_STATUS_CONTROLLER_CHANGE,
				 Channel, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ok = midi_out(ID, ?MIDI_STATUS_PROGRAM_CHANGE, 
				 Channel, 0, 0),
    ok = midi_out(ID, ?MIDI_STATUS_CONTROLLER_CHANGE,
				 Channel+1, ?MIDI_MESSAGE_BANK_MSB_CONTROL, 0),
    ok = midi_out(ID, ?MIDI_STATUS_PROGRAM_CHANGE, 
				 Channel+1, 5, 0),
    ?D(ID),
    NoteNum = 60,
    OnVelocity = 77,
    OffVelocity = 0,
    lists:foreach(fun(N) ->
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum+N, OnVelocity),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel+1, NoteNum-N, OnVelocity),
			  timer:sleep(50),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum+N, OffVelocity),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel+1, NoteNum-N, OffVelocity)
		  end, lists:seq(0, 30)),
    ok.

z() ->
    z(1.5).

z(Speed) ->
    ID = open_midi_output(0),
    ?D(ID),
    [_ | [F0]] = midi_file:read_midi_file("C:/Documents and Settings/Jakob/Skrivbord/jazz.mid"),
%%     [_ | [F0]] = midi_file:read_midi_file("C:/Documents and Settings/Jakob/Skrivbord/test.mid"),
%%     F = transpose(F0, -1),
    F = F0,
    lists:foreach(fun({Delay, {Status, Channel, Note, Velocity}}) when Status=:=on; Status=:=off ->
			  StatusNum = midi:get_status_const(Status),
			  ok = midi_out(ID, StatusNum, Channel, Note, Velocity),
			  timer:sleep(trunc(Delay*Speed));
		     ({Delay, {program_change, Channel, P}}) ->
			  ok = midi_out(ID, ?MIDI_STATUS_PROGRAM_CHANGE, Channel, P, 0),
			  timer:sleep(trunc(Delay*Speed));
		     ({Delay, {controller_change, Channel, Controller, Value}}) ->
			  ControllerNum = midi:get_controller_const(Controller),
			  ok = midi_out(ID, ?MIDI_STATUS_CONTROLLER_CHANGE, Channel,
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
    ID = open_midi_output(1),
    NoteNum = 60,
    Channel = 0,
    OnVelocity = 50,
    OffVelocity = 0,
    lists:foreach(fun(N) ->
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum+N, OnVelocity),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum-N, OnVelocity),
			  timer:sleep(50),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum+N, OffVelocity),
			  ok = midi_out(ID, ?MIDI_STATUS_ON, Channel, NoteNum-N, OffVelocity)
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

