%% Author: jakob (jakobce@gmail.com)
%% Created: 3 mar 2009
%% Description: a simple midi-file reader

-module(midi_file).

%%
%% Include files
%%

-include("midi.hrl").

%%
%% Exported Functions
%%

-export([read_midi_file/1]).

%%
%% API Functions
%%

read_midi_file(Filename) when is_list(Filename) ->
    {ok, B} = file:read_file(Filename),
    get_chunks(B, []).

%%
%% Local Functions
%%

%% took most of it from http://crystal.apana.org.au/ghansper/midi_introduction/midi_file_format.html

get_ticks(<<1:1, SMPTEFrames:7/integer-unsigned, TicksPerFrame:16/integer-unsigned>>) ->
    Milliseconds = case SMPTEFrames of
		       128-29 ->
			   29.97 * TicksPerFrame;
		       _ ->
			   (128-SMPTEFrames) * TicksPerFrame
		   end,
    {milliseconds, Milliseconds};
get_ticks(<<0:1, TicksPerQuarter:15/integer-unsigned>>) ->
    {ticks_per_quarter_note, TicksPerQuarter}.

get_chunk(<<"MThd">>, <<FormatType:16/integer, 
			NTracks:16/integer, TimeDiv:2/binary, _Ignored/binary>>) ->
    #midi_file_header{format_type=FormatType,
		      n_tracks=NTracks, 
		      time_division=get_ticks(TimeDiv)};
get_chunk(<<"MTrk">>, Data) ->
    get_events(Data).

get_events(Data) ->
    get_events(Data, <<>>, []).

get_events(<<>>, _Status, Acc) ->
    lists:reverse(Acc);
get_events(DataWTime, Status, Acc) ->
    {Delta, Data} = midi:get_var_length(DataWTime),
    case get_event_w_running_status(Data, Status) of
	{NewStatus, {Event, Rest}} ->
	    get_events(Rest, NewStatus, [{Delta, Event} | Acc]);
	{Event, Rest} ->
	    get_events(Rest, Status, [{Delta, Event} | Acc])
    end.

get_meta_event_data(sequence_number, <<S:16/integer-unsigned-big, _/binary>>) ->
    S;
get_meta_event_data(set_tempo, <<Tempo:24/integer-unsigned-big, _/binary>>) ->
    Tempo;
get_meta_event_data(offset, <<Hours, Minutes, Seconds, Frames, Fractions, _/binary>>) ->
    {Hours, Minutes, Seconds, Frames, Fractions};
get_meta_event_data(midi_port_old, <<Port>>) ->
    Port;
get_meta_event_data(end_of_track, _) ->
    ok;
get_meta_event_data(time_signature, <<Numerator, DenominatorExp2, TicksPerMetro, ThirtyTwos, _/binary>>) ->
    {{Numerator, 1 bsl DenominatorExp2}, TicksPerMetro, ThirtyTwos};
get_meta_event_data(key_signature, <<Sharps:8/integer-signed, Minor, _/binary>>) ->
    {midi:sharps_to_key(Sharps), case Minor of 0 -> major; 1 -> minor end};
get_meta_event_data(_MetaEvent, Data) ->
    binary_to_list(Data).

get_event_w_running_status(<<Status, Rest/binary>>, _RunningStatus) when Status>=16#80 ->
    {Status, get_event(<<Status>>, Rest)};
get_event_w_running_status(Data, RunningStatus) ->
    get_event(<<RunningStatus>>, Data).

get_chunks(<<>>, Acc) ->
    lists:reverse(Acc);
get_chunks(<<Type:4/binary, Len:32/integer-unsigned, Data:Len/binary, Rest/binary>>, Acc) ->
    Chunk = get_chunk(Type, Data),
    get_chunks(Rest, [Chunk | Acc]).

get_event(<<?MIDI_STATUS_META>>, <<MetaType, Data/binary>>) ->
    {Len, Rest0} = midi:get_var_length(Data),
    <<EventData:Len/binary, Rest1/binary>> = Rest0,
    MetaName = midi:get_meta_name(MetaType),
    MetaEvent = {meta_event, MetaName, get_meta_event_data(MetaName, EventData)},
    {MetaEvent, Rest1};
get_event(Status, Data) ->
    midi:get_event(Status, Data).




