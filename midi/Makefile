eversion = 3.8.1
edir = /opt/local/lib/erlang
fdir = /System/Library/Frameworks
src_dir = c_src
odir = $(src_dir)

all:	core_midi

core_midi:	$(src_dir)/erl_mac.o

$(src_dir)/erl_mac.o:
	gcc \
	    -L $(edir)/usr/lib \
	    -L $(edir)/lib/erl_interface-$(eversion)/lib \
	    -I $(edir)/usr/include \
	    -I $(edir)/lib/erl_interface-$(eversion)/include \
	    -I $(fdir)/AudioUnit.framework/Headers \
	    -I $(fdir)/AudioToolbox.framework/Headers \
	    -I $(fdir)/CoreServices.framework/Headers \
	    -I $(fdir)/CoreMIDI.framework/Headers \
	    -flat_namespace -fPIC -undefined dynamic_lookup \
	    -framework AudioUnit -framework AudioToolbox -framework CoreServices \
	    -framework CoreMIDI -shared \
	    -l ei \
	    -o $(odir)/erl_mac.o \
	    c_src/erl_midi_mac.cpp

clean:
	rm -rf $(c_src)/*.o
