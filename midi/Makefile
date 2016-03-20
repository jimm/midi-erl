all:
	gcc -v -o erl_midi_mac \
	    -I /System/Library/Frameworks/CoreMIDI.framework/Headers/ \
	    -I /System/Library/Frameworks/AudioToolbox.framework/Headers/ \
	    -I /System/Library/Frameworks/AudioUnit.framework/Headers/ \
	    -I /System/Library/Frameworks/CoreServices.framework/Headers/ \
	    -I /opt/local/lib/erlang/usr/include \
	    -framework AudioToolbox \
	    -framework CoreMIDI \
	    -framework AudioUnit \
	    -framework CoreServices \
	    c_src/erl_midi_mac.cpp

# all:
# 	cc  -I /System/Library/Frameworks/CoreMIDI.framework/Headers/ \
# 	    -I /System/Library/Frameworks/AudioToolbox.framework/Headers/ \
#             -I /System/Library/Frameworks/AudioUnit.framework/Headers/ \
#             -I /System/Library/Frameworks/CoreServices.framework/Headers/ \
#             -I /opt/local/lib/erlang/usr/include \
#             c_src/erl_midi_mac.cpp
