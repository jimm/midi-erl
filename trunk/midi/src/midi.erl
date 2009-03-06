%% Author: jakob
%% Created: 3 mar 2009
%% Description: some midi-routines

-module(midi).

%%
%% Include files
%%

-include("midi.hrl").

%%
%% Exported Functions
%%

-export([get_var_length/1]).
-export([get_meta_name/1, get_status_name/1, get_controller_name/1]).
-export([sharps_to_key/1, get_event/2]).

meta_nums() ->
    [{?MIDI_META_SEQUENCE_NUMBER, sequence_number},
     {?MIDI_META_TEXT_EVENT, text_event},
     {?MIDI_META_COPYRIGHT_NOTICE, copyright_notice},
     {?MIDI_META_SEQUENCE_TRACK_NAME, sequence_track_name},
     {?MIDI_META_INSTRUMENT_NAME, instrument_name},
     {?MIDI_META_LYRIC, lyric},
     {?MIDI_META_MARKER, marker},
     {?MIDI_META_CUE_POINT, cue_point},
     {?MIDI_META_MIDI_PORT_OLD, midi_port_old},
     {?MIDI_META_END_OF_TRACK, end_of_track},
     {?MIDI_META_SET_TEMPO, set_tempo},
     {?MIDI_META_SMTPE_OFFSET, smtpe_offset},
     {?MIDI_META_TIME_SIGNATURE, time_signature},
     {?MIDI_META_KEY_SIGNATURE, key_signature}].


controller_nums() ->
    [{?MIDI_CONTROLLER_BANK_SELECT, bank_select},
     {?MIDI_CONTROLLER_MODULATION_WHEEL, modulation_wheel},
     {?MIDI_CONTROLLER_BREATH_CONTROLLER, breath_controller},
     {?MIDI_CONTROLLER_FOOT_CONTROLLER, foot_controller},
     {?MIDI_CONTROLLER_PORTAMENTO_TIME, portamento_time},
     {?MIDI_CONTROLLER_DATA_ENTRY_MSB, data_entry_msb},
     {?MIDI_CONTROLLER_MAIN_VOLUME, main_volume},
     {?MIDI_CONTROLLER_BALANCE, balance},
     {?MIDI_CONTROLLER_PAN, pan},
     {?MIDI_CONTROLLER_EXPRESSION, expression},
     {?MIDI_CONTROLLER_EFFECT_CONTROL_1, effect_control_1},
     {?MIDI_CONTROLLER_EFFECT_CONTROL_2, effect_control_2},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_1, general_purpose_1},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_2, general_purpose_2},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_3, general_purpose_3},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_4, general_purpose_4},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_0, controller_lsb_0},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_1, controller_lsb_1},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_2, controller_lsb_2},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_3, controller_lsb_3},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_4, controller_lsb_4},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_5, controller_lsb_5},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_6, controller_lsb_6},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_7, controller_lsb_7},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_8, controller_lsb_8},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_9, controller_lsb_9},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_10, controller_lsb_10},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_11, controller_lsb_11},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_12, controller_lsb_12},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_13, controller_lsb_13},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_14, controller_lsb_14},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_15, controller_lsb_15},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_16, controller_lsb_16},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_17, controller_lsb_17},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_18, controller_lsb_18},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_19, controller_lsb_19},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_20, controller_lsb_20},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_21, controller_lsb_21},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_22, controller_lsb_22},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_23, controller_lsb_23},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_24, controller_lsb_24},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_25, controller_lsb_25},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_26, controller_lsb_26},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_27, controller_lsb_27},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_28, controller_lsb_28},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_29, controller_lsb_29},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_30, controller_lsb_30},
     {?MIDI_CONTROLLER_CONTROLLER_LSB_31, controller_lsb_31},
     {?MIDI_CONTROLLER_DAMPER_PEDAL, damper_pedal},
     {?MIDI_CONTROLLER_PORTAMENTO, portamento},
     {?MIDI_CONTROLLER_SOSTENUTO, sostenuto},
     {?MIDI_CONTROLLER_SOFT_PEDAL, soft_pedal},
     {?MIDI_CONTROLLER_LEGATO_FOOTSWITCH, legato_footswitch},
     {?MIDI_CONTROLLER_HOLD_2, hold_2},
     {?MIDI_CONTROLLER_SOUND_VARIATION, sound_variation},
     {?MIDI_CONTROLLER_TIMBRE, timbre},
     {?MIDI_CONTROLLER_RELEASE_TIME, release_time},
     {?MIDI_CONTROLLER_ATTACK_TIME, attack_time},
     {?MIDI_CONTROLLER_BRIGHTNESS, brightness},
     {?MIDI_CONTROLLER_SOUND_CONTROLLER_6, sound_controller_6},
     {?MIDI_CONTROLLER_SOUND_CONTROLLER_7, sound_controller_7},
     {?MIDI_CONTROLLER_SOUND_CONTROLLER_8, sound_controller_8},
     {?MIDI_CONTROLLER_SOUND_CONTROLLER_9, sound_controller_9},
     {?MIDI_CONTROLLER_SOUND_CONTROLLER_10, sound_controller_10},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_5, general_purpose_5},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_6, general_purpose_6},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_7, general_purpose_7},
     {?MIDI_CONTROLLER_GENERAL_PURPOSE_8, general_purpose_8},
     {?MIDI_CONTROLLER_PORTAMENTO_CONTROL, portamento_control},
     {?MIDI_CONTROLLER_EFFECTS_DEPTH_1, effects_depth_1},
     {?MIDI_CONTROLLER_EFFECTS_DEPTH_2, effects_depth_2},
     {?MIDI_CONTROLLER_EFFECTS_DEPTH_3, effects_depth_3},
     {?MIDI_CONTROLLER_EFFECTS_DEPTH_4, effects_depth_4},
     {?MIDI_CONTROLLER_EFFECTS_DEPTH_5, effects_depth_5},
     {?MIDI_CONTROLLER_DATA_INCREMENT, data_increment},
     {?MIDI_CONTROLLER_DATA_DECREMENT, data_decrement},
     {?MIDI_CONTROLLER_NON_REGISTERED_PARAMETER_NUMBER_LSB, non_registered_parameter_number_lsb},
     {?MIDI_CONTROLLER_NON_REGISTERED_PARAMETER_NUMBER_MSB, non_registered_parameter_number_msb},
     {?MIDI_CONTROLLER_REGISTERED_PARAMETER_NUMBER_LSB, registered_parameter_number_lsb},
     {?MIDI_CONTROLLER_REGISTERED_PARAMETER_NUMBER_MSB, registered_parameter_number_msb}].

status_nums() ->
    [{?MIDI_STATUS_OFF, off},
     {?MIDI_STATUS_ON, on},
     {?MIDI_STATUS_POLY_AFTERTOUCH, poly_aftertouch},
     {?MIDI_STATUS_CONTROLLER_CHANGE, controller_change},
     {?MIDI_STATUS_PROGRAM_CHANGE, program_change},
     {?MIDI_STATUS_AFTERTOUCH, aftertouch},
     {?MIDI_STATUS_PITCH_BEND, pitch_bend},
     {?MIDI_STATUS_SYSEX, sysex},
     {?MIDI_STATUS_SONG_POINTER, song_pointer},
     {?MIDI_STATUS_SYSEX_F7, sysex_f7},
     {?MIDI_STATUS_META, meta}].

name_conv(N, L) when is_integer(N) ->
    case lists:keysearch(N, 1, L) of
	{value, {_, A}} ->
	    A;
	false ->
	    N
    end;
name_conv(A, L) when is_atom(A) ->
    case lists:keysearch(A, 2, L) of
	{value, {N, _}} ->
	    N;
	false ->
	    A
    end.

get_meta_name(M) ->
    name_conv(M, meta_nums()).

get_status_name(M) ->
    name_conv(M, status_nums()).

get_controller_name(M) ->
    name_conv(M, controller_nums()).

keys() ->
    [g_flat, d_flat, a_flat, e_flat, b_flat, f, c, g, d, a, e, b, f_sharp].

sharps_to_key(N) when N >=-7, N =< 7 ->
    lists:nth(N+7, keys()).

get_var_length(<<0:1, N:7/integer-unsigned, Rest/binary>>) ->
    {N, Rest};
get_var_length(<<1:1, N1:7/integer-unsigned, 0:1, N2:7/integer-unsigned, Rest/binary>>) ->
    {N1 bsl 7 + N2, Rest};
get_var_length(<<1:1, N1:7/integer-unsigned, 1:1, N2:7/integer-unsigned, 0:1, 
		 N3:7/integer, Rest/binary>>) ->
    {N1 bsl 14 + N2 bsl 7 + N3, Rest};
get_var_length(<<1:1, N1:7/integer-unsigned, 1:1, N2:7/integer-unsigned, 1:1, 
		 N3:7/integer-unsigned, 0:1, N4:7/integer-unsigned, Rest/binary>>) ->
    {N1 bsl 21 + N2 bsl 14 + N3 bsl 7 + N4, Rest}.

get_ev_controller_par(?MIDI_STATUS_CONTROLLER_CHANGE, A) ->
    get_controller_name(A);
get_ev_controller_par(_, A) ->
    A.

%% get a MIDI event
get_event(<<Sysex:8>>, <<Data/binary>>) % var. length parameter
  when Sysex =:= ?MIDI_STATUS_SYSEX; Sysex =:= ?MIDI_STATUS_SYSEX_F7; Sysex =:= ?MIDI_STATUS_SONG_POINTER ->
    {Len, Rest0} = get_var_length(Data),
    <<EventData:Len/binary, Rest1/binary>> = Rest0,
    Name = midi:get_status_name(Sysex),
    {{Name, EventData}, Rest1};
get_event(<<N:4, Channel:4>>, <<A, B, Rest/binary>>) % two bytes parameter
  when N =:= ?MIDI_STATUS_OFF; N =:= ?MIDI_STATUS_ON; N =:= ?MIDI_STATUS_POLY_AFTERTOUCH;
       N =:= ?MIDI_STATUS_CONTROLLER_CHANGE; N =:= ?MIDI_STATUS_PITCH_BEND ->
    Name = midi:get_status_name(N),
    Par = get_ev_controller_par(N, A),
    {{Name, Channel, Par, B}, Rest};
get_event(<<N:4, Channel:4>>, <<A, Rest/binary>>) % one byte parameter
  when N =:= ?MIDI_STATUS_PROGRAM_CHANGE; N =:= ?MIDI_STATUS_AFTERTOUCH ->
    Name = midi:get_status_name(N),
    {{Name, Channel, A}, Rest}.
