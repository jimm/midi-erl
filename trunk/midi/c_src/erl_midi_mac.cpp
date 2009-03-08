/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 *
 *     $Id$
 */

/*
 * Purpose: A driver using libpq to connect to Postgres
 * from erlang, a sample for the driver documentation
 */

#include <erl_driver.h>
#include <ei.h>

#include <CoreServices.h> //for file stuff
#include <AudioUnit.h>
#include <AudioToolbox.h> //for AUGraph
#include <CoreMIDI.h>

using namespace std;

#include <iostream>

#if DEBUG
#define L fprintf(stderr, "%s:%d\r\n", __FILE__, __LINE__);
#define D(fmt, d) (fprintf(stderr, "%s:%d\t", __FILE__, __LINE__), fprintf(stderr, fmt, d), fprintf(stderr, "\r\n"))
#else
#define L
#define D(fmt, d)
#endif
#define DD(fmt, d)

/* Driver interface declarations */
static ErlDrvData start(ErlDrvPort port, char *command);
static void stop(ErlDrvData drv_data);
static int control(ErlDrvData drv_data, unsigned int command, char *buf,
	int len, char **rbuf, int rlen);

static ErlDrvEntry erl_midi_driver_entry = { //
	NULL, /* init */
	start, //
	stop, //
	NULL, // output
	NULL, /* ready_input */
	NULL, /* ready_output */
	"erl_midi", // the name of the driver
	NULL, /* finish */
	NULL, /* handle */
	control, /* control */
	NULL, /* timeout */
	NULL, /* outputv */
	NULL, // ready_async
	NULL, /* flush */
	NULL, /* call */
	NULL /* event */
};

/* INITIALIZATION AFTER LOADING */

/*
 * This is the init function called after this driver has been loaded.
 * It must *not* be declared static. Must return the address to
 * the driver entry.
 */

#ifdef __cplusplus
extern "C" { // shouldn't this be in the DRIVER_INIT macro?
#endif
DRIVER_INIT(erl_midi)
{
    return &erl_midi_driver_entry;
}
#ifdef __cplusplus
}
#endif

struct our_data_t
{
    ErlDrvPort port;
    ei_x_buff x;
};

/* DRIVER INTERFACE */
static ErlDrvData start(ErlDrvPort port, char *)
{
    our_data_t* data;
    data = (our_data_t*)driver_alloc(sizeof(our_data_t));
    data->port = port;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return reinterpret_cast<ErlDrvData> (data);
}

enum
{
    DRV_SET_SOUND_BANK = 1,
    DRV_MUSIC_DEVICE_MIDI_EVENT,
    DRV_DEVICE_LIST,
    DRV_SOURCE_LIST,
    DRV_DESTINATION_LIST,
    DRV_CREATE_CLIENT,
    DRV_CONNECT_SOURCE,
    DRV_CREATE_INPUT_PORT,
    DRV_CREATE_OUTPUT_PORT,
    DRV_SEND_MIDI,
    DRV_NOW,
    DRV_TEST,

    DRV_AU_GRAPH_INITIALIZE,
    DRV_AU_GRAPH_START,
    DRV_NEW_AU_GRAPH,
    DRV_AU_GRAPH_ADD_NODE,
    DRV_AU_GRAPH_OPEN,
    DRV_AU_GRAPH_CONNECT_NODE_INPUT,
    DRV_AU_GRAPH_NODE_INFO,
    DRV_MUSIC_DEVICE_MIDI_SYS_EX,

    DRV_DISPOSE_CLIENT,
    DRV_DISPOSE_PORT,

    DRV_DISPOSE_AU_GRAPH
};

//static void output(our_data_t* data, ei_x_buff* x)
//{
//    driver_output(data->port, x->buff, x->index);
//}

static ErlDrvBinary* ei_x_to_new_binary(ei_x_buff* x)
{
    ErlDrvBinary* bin = driver_alloc_binary(x->index);
    if (bin != NULL)
	memcpy(&bin->orig_bytes[0], x->buff, x->index);
    return bin;
}

static void do_test(our_data_t* data);
static void do_list_midi_objects(our_data_t* data, int command);
static void do_create_client(our_data_t* data, char* buf, int len);
static void do_connect_source(our_data_t* data, char* buf, int len);
static void do_create_input_port(our_data_t* data, char* buf, int len);
static void do_create_output_port(our_data_t* data, char* buf, int len);
static void do_send_midi(our_data_t* data, char* buf, int len);
static void do_music_device_midi_event(our_data_t* data, char* buf, int len);
//static void do_create_au_graph(our_data_t* data);
static void do_now(our_data_t* data);

static void do_au_graph_initialize(our_data_t* data, char* buf, int len);
static void do_new_au_graph(our_data_t* data);
static void do_au_graph_start(our_data_t* data, char* buf, int len);
static void do_au_graph_add_node(our_data_t* data, char* buf, int len);
static void do_au_graph_open(our_data_t* data, char* buf, int len);
static void do_au_graph_connect_node_input(our_data_t* data, char* buf, int len);
static void do_au_graph_node_info(our_data_t* data, char* buf, int len);
static void do_music_device_midi_sys_ex(our_data_t* data, char* buf, int len);
static void do_dispose(our_data_t* data, char* buf, int len, int command);

static int control(ErlDrvData drv_data, unsigned int command, char *buf,
	int len, char **rbuf, int rlen)
{
    int r = 0;
    //   char* s = get_s(buf, len);
    DD("command %d", command);
    our_data_t* data = reinterpret_cast<our_data_t*> (drv_data);
    ei_x_new_with_version(&data->x);
    switch (command) {
    case DRV_TEST:
	do_test(data);
	break;
//    case DRV_CREATE_AU_GRAPH:
//	do_create_au_graph(data);
//	break;

    case DRV_NEW_AU_GRAPH:
	do_new_au_graph(data);
	break;
    case DRV_AU_GRAPH_ADD_NODE:
	do_au_graph_add_node(data, buf, len);
	break;
    case DRV_AU_GRAPH_OPEN:
	do_au_graph_open(data, buf, len);
	break;
    case DRV_AU_GRAPH_INITIALIZE:
	do_au_graph_initialize(data, buf, len);
	break;
    case DRV_AU_GRAPH_START:
	do_au_graph_start(data, buf, len);
	break;
    case DRV_AU_GRAPH_CONNECT_NODE_INPUT:
	do_au_graph_connect_node_input(data, buf, len);
	break;
    case DRV_AU_GRAPH_NODE_INFO:
	do_au_graph_node_info(data, buf, len);
	break;

    case DRV_SET_SOUND_BANK:
    case DRV_DEVICE_LIST:
    case DRV_SOURCE_LIST:
    case DRV_DESTINATION_LIST:
	do_list_midi_objects(data, command);
	break;
	//    case DRV_DISCONNECT:
	//	r = do_disconnect((our_data_t*)drv_data);
	//	break;
	//    case DRV_SELECT:
	//	r = do_select(s, (our_data_t*)drv_data);
	//	break;
    case DRV_CREATE_CLIENT:
	do_create_client(data, buf, len);
	break;
    case DRV_CONNECT_SOURCE:
	do_connect_source(data, buf, len);
	break;
    case DRV_CREATE_INPUT_PORT:
	do_create_input_port(data, buf, len);
	break;
    case DRV_CREATE_OUTPUT_PORT:
	do_create_output_port(data, buf,
		len);
	break;
    case DRV_SEND_MIDI:
	do_send_midi(data, buf, len);
	break;
    case DRV_MUSIC_DEVICE_MIDI_EVENT:
	do_music_device_midi_event(data, buf, len);
	break;
    case DRV_MUSIC_DEVICE_MIDI_SYS_EX:
	do_music_device_midi_sys_ex(data, buf, len);
	break;
    case DRV_NOW:
	do_now(data);
	break;
    case DRV_DISPOSE_CLIENT:
    case DRV_DISPOSE_PORT:
    case DRV_DISPOSE_AU_GRAPH:
	do_dispose(data, buf, len, command);
	break;
    default:
	r = -1;
	break;
    }
    if (r == 0) {
	*rbuf = reinterpret_cast<char*> (ei_x_to_new_binary(&data->x));
    }
    ei_x_free(&data->x);
    //    free_s(s);
    return r;
}

static void stop(ErlDrvData drv_data)
{
    //    do_disconnect((our_data_t*)drv_data);
}

static int encode_timestamp(ei_x_buff* x, MIDITimeStamp t)
{
    return ei_x_encode_binary(x, &t, sizeof(t));
}

static MIDITimeStamp decode_timestamp(char* buf, int* index)
{
    MIDITimeStamp t;
    long len = sizeof(t);
    ei_decode_binary(buf, index, &t, &len);
    return t;
}

static void do_now(our_data_t* data)
{
    ei_x_buff* x = &data->x;
    MIDITimeStamp now = AudioGetCurrentHostTime();
    encode_timestamp(x, now);
}

static void do_test(our_data_t* data)
{
    ei_x_buff* x = &data->x;
    ei_x_encode_list_header(x, 2);
    ei_x_encode_atom(x, "ok");
    ei_x_encode_string(x, "test");
    ei_x_encode_empty_list(x);
}

//static void encode_midi_id(ei_x_buff* x, MIDIObjectRef obj)
//{
//    MIDIUniqueID id;
//    OSStatus err = MIDIObjectGetIntegerProperty(obj, kMIDIPropertyUniqueID, &id);
//    if (err == noErr)
//	ei_x_encode_ulong(x, id);
//    else
//	ei_x_encode_ulong(x, 0);
//}

enum PutGet
{
    pgPut, pgGet
};

static void PutGetCFString(std::string& s, CFStringRef& cs, PutGet pg)
{
    if (pg == pgPut) {
	if (cs != NULL)
	    CFRelease(cs);
	cs = CFStringCreateWithCString(kCFAllocatorDefault, s.c_str(),
		kCFStringEncodingUTF8);
    } else {
	if (cs == NULL) {
	    s.clear();
	} else {
	    const char* p = CFStringGetCStringPtr(cs, kCFStringEncodingUTF8);
	    if (p == NULL) {
		int n = CFStringGetLength(cs) * 4;
		char* buf = new char[n];
		CFStringGetCString(cs, buf, n, kCFStringEncodingUTF8);
		s = buf;
		delete[] buf;
	    } else
		s = p;
	}
    }
}

static int encode_CFString(ei_x_buff* x, CFStringRef& cs)
{
    std::string s;
    PutGetCFString(s, cs, pgGet);
    DD("s '%s'", s.c_str());DD("(len %d)", s.length());
    return ei_x_encode_string(x, s.c_str());
}

static CFStringRef decode_CFString(char* buf, int* index)
{
    int type, len;
    ei_get_type(buf, index, &type, &len);
    char* p = new char[len + 1];
    ei_decode_string(buf, index, p);
    std::string s = p;
    delete[] p;
    CFStringRef cs = NULL;
    PutGetCFString(s, cs, pgPut);
    return cs;
}

static void encode_mac_error(ei_x_buff* x, OSStatus err)
{
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, "error");
    ei_x_encode_long(x, err);
}

static void encode_tupled_binary(ei_x_buff* x, const char* name, void* data, int size, OSStatus err)
{
    if (err == noErr) {
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_atom(x, name);
	ei_x_encode_binary(x, data, size);
    } else
	encode_mac_error(x, err);
}

static void encode_midi_obj(ei_x_buff* x, MIDIObjectRef obj)
{
    encode_tupled_binary(x,"MIDIObject", &obj, sizeof(obj), noErr);
}

static bool decode_tupled_binary(char* buf, int* index, char* name, int size, void* result)
{
    int arity;
    if (ei_decode_tuple_header(buf, index, &arity) != 0)
	return false;
    if (arity != 2)
	return false;
    char atom[MAXATOMLEN];
    if (ei_decode_atom(buf, index, atom) != 0)
	return false;
    if (strcmp(atom, name) != 0)
	return false;
    long sz;
    int i = *index;
    if (ei_decode_binary(buf, &i, NULL, &sz) != 0)
	return false;
    if (sz != size)
	return false;
    if (ei_decode_binary(buf, index, result, &sz) != 0)
	return false;
    return true;
}

static bool decode_midi_obj(char* buf, int* index, MIDIObjectRef* obj)
{
    return decode_tupled_binary(buf, index, "MIDIObject", sizeof(*obj), obj);
}

static void encode_midi_object(ei_x_buff* x, MIDIObjectRef obj)
{
    OSStatus err = -1;
    if (obj == NULL)
	goto error;
    ei_x_encode_tuple_header(x, 2);
    encode_midi_obj(x, obj);
//    encode_midi_id(x, obj);
    {
	CFStringRef pname = NULL;
	err = MIDIObjectGetStringProperty(obj, kMIDIPropertyName, &pname);
	DD("e %d\n", e);DD("pname %d\n", pname);
	if (err != noErr)
	    goto error;
	encode_CFString(x, pname);
	CFRelease(pname);
    }
    error: ;
    ei_x_encode_empty_list(x);
}

// no arguments
static void do_list_midi_objects(our_data_t* data, int command)
{
    ei_x_buff* x = &data->x;
    int n = 0;
    switch (command) {
    case DRV_DEVICE_LIST:
	n = MIDIGetNumberOfDevices();
	break;
    case DRV_SOURCE_LIST:
	n = MIDIGetNumberOfSources();
	break;
    case DRV_DESTINATION_LIST:
	n = MIDIGetNumberOfDestinations();
	break;
    }
    ei_x_encode_list_header(x, n);
    for (int i = 0; i < n; ++i) {
	MIDIObjectRef obj = NULL;
	switch (command) {
	case DRV_DEVICE_LIST:
	    obj = MIDIGetDevice(i);
	    break;
	case DRV_SOURCE_LIST:
	    obj = MIDIGetSource(i);
	    break;
	case DRV_DESTINATION_LIST:
	    obj = MIDIGetDestination(i);
	    break;
	}
	encode_midi_object(x, obj);
    }
    ei_x_encode_empty_list(x);
}

static void encode_midi_object(ei_x_buff* x, MIDIObjectRef obj, OSStatus err)
{
    if (err == noErr)
	encode_midi_object(x, obj);
    else
	encode_mac_error(x, err);
}

static void encode_au_graph(ei_x_buff* x, AUGraph graph, OSStatus err)
{
    encode_tupled_binary(x, "AUGraph", &graph, sizeof(graph), err);
}

static void encode_au_node(ei_x_buff* x, AUNode node, OSStatus err)
{
    encode_tupled_binary(x, "AUNode", &node, sizeof(node), err);
}

static void encode_audio_unit(ei_x_buff* x, AudioUnit unit, OSStatus err)
{
    encode_tupled_binary(x, "AudioUnit", &unit, sizeof(unit), err);
}

static void encode_ok_or_error(ei_x_buff* x, OSStatus err)
{
    if (err == noErr)
	ei_x_encode_atom(x, "ok");
    else
	encode_mac_error(x, err);
}

/*
should be a record
struct ComponentDescription {
   OSType componentType;
   OSType componentSubType;
   OSType componentManufacturer;
   unsigned long componentFlags;
   unsigned long componentFlagsMask;
};
typedef struct ComponentDescription ComponentDescription;
 *
 */
static int decode_component_description(char* buf, int* index, ComponentDescription& cd)
{
    int arity;
    if (ei_decode_tuple_header(buf, index, &arity) != 0)
	return -10;
    if (arity != 6)
	return -arity;
    unsigned long n;
    char atom[MAXATOMLEN];
    if (ei_decode_atom(buf, index, atom) != 0)
	return -12;
    if (strcmp(atom, "ComponentDescription") != 0)
	return -13;
    if (ei_decode_ulong(buf, index, &n) != 0)
	return -14;
    cd.componentType = n;
    if (ei_decode_ulong(buf, index, &n) != 0)
	return -15;
    cd.componentSubType = n;
    if (ei_decode_ulong(buf, index, &n) != 0)
	return -16;
    cd.componentManufacturer = n;
    if (ei_decode_ulong(buf, index, &n) != 0)
	return -17;
    cd.componentFlags = n;
    if (ei_decode_ulong(buf, index, &n) != 0)
	return -18;
    cd.componentFlagsMask = n;
    return noErr;
}

static void encode_component_description(ei_x_buff* x, const ComponentDescription& cd)
{
    ei_x_encode_tuple_header(x, 6);
    ei_x_encode_atom(x, "ComponentDescription");
    ei_x_encode_ulong(x, cd.componentType);
    ei_x_encode_ulong(x, cd.componentSubType);
    ei_x_encode_ulong(x, cd.componentManufacturer);
    ei_x_encode_ulong(x, cd.componentFlags);
    ei_x_encode_ulong(x, cd.componentFlagsMask);
}


static bool decode_au_graph(char* buf, int* index, AUGraph& graph)
{
    return decode_tupled_binary(buf, index, "AUGraph", sizeof(graph), &graph);
}

static bool decode_audio_unit(char* buf, int* index, AudioUnit& unit)
{
    return decode_tupled_binary(buf, index, "AudioUnit", sizeof(unit), &unit);
}

static bool decode_au_node(char* buf, int* index, AUNode& node)
{
    return decode_tupled_binary(buf, index, "AUNode", sizeof(node), &node);
}


static void do_new_au_graph(our_data_t* data)
{
    AUGraph graph;
    OSStatus err = NewAUGraph(&graph);
    ei_x_buff* x = &data->x;
    if (err == noErr) {
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_atom(x, "ok");
    }
    encode_au_graph(x, graph, err);
}

static bool decode_version_and_tuple_header(char* buf, int* index, int arity_wanted)
{
    int version, arity;
    if (ei_decode_version(buf, index, &version) != 0)
	return false;
    if (ei_decode_tuple_header(buf, index, &arity) != 0)
	return false;
    return arity == arity_wanted;
}

/*
extern OSStatus
AUGraphNodeInfo(AUGraph	inGraph,
		AUNode			inNode,
		ComponentDescription *	outDescription,
		AudioUnit *		outAudioUnit)			AVAILABLE_MAC_OS_X_VERSION_10_5_AND_LATER;
 */
static void do_au_graph_node_info(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0;
    if (!decode_version_and_tuple_header(buf, &index, 2))
	goto error;
    AUGraph graph;
    AUNode node;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    if (!decode_au_node(buf, &index, node))
	goto error;
    ComponentDescription cd;
    AudioUnit audioUnit;
    err = AUGraphNodeInfo(graph, node, &cd, &audioUnit);
    if (err != noErr)
	goto error;
    ei_x_buff* x = &data->x;
    ei_x_encode_tuple_header(x, 3);
    ei_x_encode_atom(x, "ok");
    encode_component_description(x, cd);
    encode_audio_unit(x, audioUnit, err);
    return;
    error: ;
    encode_mac_error(&data->x, err);
}

static void do_au_graph_add_node(our_data_t* data, char* buf, int len)
{
    ComponentDescription cd;
    AUGraph graph;
    AUNode node;
    OSStatus err = -1;
    int index = 0;
    if (!decode_version_and_tuple_header(buf, &index, 2))
	goto error;
    err = -3;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    err = decode_component_description(buf, &index, cd);
    if (err != noErr)
	goto error;
    err = AUGraphAddNode(graph, &cd, &node);
    error: ;
    ei_x_buff* x = &data->x;
    if (err == noErr) {
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_atom(x, "ok");
    }
    encode_au_node(x, node, err);
}

static void do_au_graph_open(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    AUGraph graph;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    err = AUGraphOpen(graph);
    error: ;
    encode_ok_or_error(&data->x, err);
}

/*
extern OSStatus
AUGraphConnectNodeInput(AUGraph	inGraph,
			AUNode	inSourceNode,
			UInt32	inSourceOutputNumber,
			AUNode	inDestNode,
			UInt32	inDestInputNumber)	AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER;

 */
static void do_au_graph_connect_node_input(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0;
    if (!decode_version_and_tuple_header(buf, &index, 5))
	goto error;
    AUGraph graph;
    AUNode sourceNode, destNode;
    unsigned long sourceOutputNumber, destInputNumber;
    err = -2;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    err = -3;
    if (!decode_au_node(buf, &index, sourceNode))
	goto error;
    err = -4;
    if (ei_decode_ulong(buf, &index, &sourceOutputNumber) != 0)
	goto error;
    err = -5;
    if (!decode_au_node(buf, &index, destNode))
	goto error;
    err = -6;
    if (ei_decode_ulong(buf, &index, &destInputNumber) != 0)
	goto error;
    err = AUGraphConnectNodeInput(graph, sourceNode, sourceOutputNumber,
	    destNode, destInputNumber);
error:
    encode_ok_or_error(&data->x, err);
}


//// This call creates the Graph and the Synth unit...
//OSStatus CreateAUGraph(AUGraph &outGraph, AudioUnit &outSynth)
//{
//    //create the nodes of the graph
//    AUNode synthNode, limiterNode, outNode;
//
//    OSStatus result = NewAUGraph(&outGraph);
//    if (result != noErr)
//	return result;
//
//    ComponentDescription cd;
//    cd.componentManufacturer = kAudioUnitManufacturer_Apple;
//    cd.componentFlags = 0;
//    cd.componentFlagsMask = 0;
//
//    cd.componentType = kAudioUnitType_MusicDevice;
//    cd.componentSubType = kAudioUnitSubType_DLSSynth;
//    result = AUGraphAddNode(outGraph, &cd, &synthNode);
//    if (result != noErr)
//	return result;
//
//    cd.componentType = kAudioUnitType_Effect;
//    cd.componentSubType = kAudioUnitSubType_PeakLimiter;
//    result = AUGraphAddNode(outGraph, &cd, &limiterNode);
//    if (result != noErr)
//	return result;
//
//    cd.componentType = kAudioUnitType_Output;
//    cd.componentSubType = kAudioUnitSubType_DefaultOutput;
//    AUGraphAddNode(outGraph, &cd, &outNode);
//    if (result != noErr)
//	return result;
//
//    result = AUGraphOpen(outGraph);
//    if (result != noErr)
//	return result;
//
//    AUGraphConnectNodeInput(outGraph, synthNode, 0, limiterNode, 0);
//    if (result != noErr)
//	return result;
//    result = AUGraphConnectNodeInput(outGraph, limiterNode, 0, outNode, 0);
//    if (result != noErr)
//	return result;
//
//    // ok we're good to go - get the Synth Unit...
//    result = AUGraphNodeInfo(outGraph, synthNode, 0, &outSynth);
//    return result;
//}

// create_client(string()) -> Client, Client=int()
static void do_create_client(our_data_t* data, char* buf, int len)
{
    // create client and ports
    MIDIClientRef client = NULL;
    int index = 0, version;
    ei_decode_version(buf, &index, &version);
    CFStringRef cs = decode_CFString(buf, &index);
    OSErr err = MIDIClientCreate(cs, NULL, NULL, &client);
    CFRelease(cs);
    ei_x_buff* x = &data->x;
    encode_midi_object(x, client, err);
}

//// create_client(string()) -> Client, Client=int()
//static void do_create_au_graph(our_data_t* data)
//{
//    AUGraph outGraph;
//    AudioUnit outSynth;
//    OSStatus err = CreateAUGraph(outGraph, outSynth);
//    ei_x_buff* x = &data->x;
//    if (err == noErr) {
//	ei_x_encode_tuple_header(x, 3);
//	ei_x_encode_atom(x, "ok");
//	encode_au_graph(x, outGraph, err);
//	encode_audio_unit(x, outSynth, err);
//    } else
//	encode_mac_error(x, err);
//}

static void do_au_graph_initialize(our_data_t* data, char* buf, int len)
{
    int index = 0, version;
    OSStatus err = -1;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    AUGraph graph;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    err = AUGraphInitialize(graph);
    error:
    encode_ok_or_error(&data->x, err);
}

static void do_au_graph_start(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    AUGraph graph;
    if (!decode_au_graph(buf, &index, graph))
	goto error;
    err = AUGraphStart(graph);
    error: ;
    encode_ok_or_error(&data->x, err);
}

static bool decode_midi_message(char* buf, int* index, int& status,
	int& param1, int& param2)
{
    long midi_message, midi_channel, p1, p2;
    if (ei_decode_long(buf, index, &midi_message) != 0)
	return false;
    if (ei_decode_long(buf, index, &midi_channel) != 0)
	return false;
    if (ei_decode_long(buf, index, &p1) != 0)
	return false;
    if (ei_decode_long(buf, index, &p2) != 0)
	return false;
    status = midi_message << 4 | midi_channel;
    param1 = p1;
    param2 = p2;
    return true;
}

static void do_music_device_midi_event(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0;
    if (!decode_version_and_tuple_header(buf, &index, 5))
	goto error;
    AudioUnit synthUnit;
    if (!decode_audio_unit(buf, &index, synthUnit))
	goto error;
    int status, param1, param2;
    if (!decode_midi_message(buf, &index, status, param1, param2))
	goto error;
    err = MusicDeviceMIDIEvent(synthUnit, status, param1, param2, 0);
    error: ;
    encode_ok_or_error(&data->x, err);
}

static void do_music_device_midi_sys_ex(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0;
    if (!decode_version_and_tuple_header(buf, &index, 2))
	goto error;
    AudioUnit synthUnit;
    if (!decode_audio_unit(buf, &index, synthUnit))
	goto error;
    long sz;
    int i = index;
    if (ei_decode_binary(buf, &i, NULL, &sz) != 0)
	goto error;
    UInt8* bin = new UInt8[sz];
    try {
	if (ei_decode_binary(buf, &index, bin, &sz) != 0)
	    goto error;
    } catch (...) {
	delete[] bin;
    }
    err = MusicDeviceSysEx(synthUnit, bin, sz);
    delete[] bin;
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}

static void MyReadProc(const MIDIPacketList *pktlist, void *refCon,
	void *connRefCon);

//static MIDIObjectRef decode_midi_object(char* buf, int* index)
//{
//    MIDIUniqueID id;
//    ei_decode_ulong(buf, index, reinterpret_cast<unsigned long*>(&id));
//    MIDIObjectType type;
//    MIDIObjectRef obj;
//    MIDIObjectFindByUniqueID(id, &obj, &type);
//    return obj;
//}

// create_input_port({Client, Name}) -> Port, Client=Port=int(), Name=string()
static void do_create_input_port(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    CFStringRef cs = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    int arity;
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
	goto error;
    err = arity;
    if (arity != 2)
	goto error;
    MIDIObjectRef obj;
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIClientRef client = reinterpret_cast<MIDIClientRef> (obj);
    cs = decode_CFString(buf, &index);
    if (cs == NULL)
	goto error;
    MIDIPortRef inPort;
    err = MIDIInputPortCreate(client, cs, MyReadProc, data, &inPort);
    CFRelease(cs);
    error: ;
    ei_x_buff* x = &data->x;
    encode_midi_object(x, inPort, err);
}

// create_output_port({Client, Name}) -> Port, Client=Port=int(), Name=string()
static void do_create_output_port(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    int arity;
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
	goto error;
    if (arity != 2)
	goto error;
    MIDIObjectRef obj;
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIClientRef client = reinterpret_cast<MIDIClientRef> (obj);
    CFStringRef cs = decode_CFString(buf, &index);
    if (cs == NULL)
	goto error;
    MIDIPortRef outPort;
    err = MIDIOutputPortCreate(client, cs, &outPort);
    CFRelease(cs);
    error: ;
    ei_x_buff* x = &data->x;
    encode_midi_object(x, outPort, err);
}

// send_midi(Port, Endpoint, Timestamp, Data) Port=Endpoint=int(), Timestamp=binary(), Data=binary()
static void do_send_midi(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    int arity;
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
	goto error;
    if (arity != 4)
	goto error;
    MIDIObjectRef obj;
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIPortRef port = reinterpret_cast<MIDIPortRef> (obj);
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIEndpointRef endpoint = reinterpret_cast<MIDIEndpointRef> (obj);
    MIDITimeStamp t = decode_timestamp(buf, &index);
    if (t == 0)
	goto error;
    int type;
    if (ei_get_type(buf, &index, &type, &len) != 0)
	goto error;
    {
	char* p = NULL, *m = NULL;
	try {
	    p = new char[len];
	    long n;
	    if (ei_decode_binary(buf, &index, p, &n) != 0)
		goto error;
	    {
		char* m = new char[len + 100];
		MIDIPacketList* pl = (MIDIPacketList*) m;
		MIDIPacket* mp = MIDIPacketListInit(pl);
		MIDIPacketListAdd(pl, len + 100, mp, t, n, (Byte*) p);
		err = MIDISend(port, endpoint, pl);
	    }
	    delete[] p;
	    delete[] m;
	} catch (...) {
	    delete[] p;
	    delete[] m;
	    throw ;
	}
    }
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}

static void MyReadProc(const MIDIPacketList *pktlist, void *refCon,
	void *connRefCon)
{
    our_data_t* data = (our_data_t*) refCon;
    MIDIPacket *packet = const_cast<MIDIPacket *> (pktlist->packet);
    ei_x_buff x;
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 2);
    ei_x_encode_atom(&x, "midi_data");
    ei_x_encode_list_header(&x, pktlist->numPackets);
    for (unsigned int j = 0; j < pktlist->numPackets; ++j) {
	ei_x_encode_tuple_header(&x, 3);
	encode_midi_obj(&x, connRefCon);
	encode_timestamp(&x, packet->timeStamp);
	ei_x_encode_binary(&x, packet->data, packet->length);
	packet = MIDIPacketNext(packet);
    }
    ei_x_encode_empty_list(&x);
    driver_output(data->port, x.buff, x.index);
    ei_x_free(&x);
}

// connect_source({Port, Endpoint}) Port=Endpoint=int()
static void do_connect_source(our_data_t* data, char* buf, int len)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    int arity;
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
	goto error;
    if (arity != 2)
	goto error;
    MIDIObjectRef obj;
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIPortRef port = (MIDIPortRef)obj;
    if (!decode_midi_obj(buf, &index, &obj))
	goto error;
    MIDIEndpointRef source = (MIDIEndpointRef) obj;
    err = MIDIPortConnectSource(port, source, port);
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}

static void do_dispose(our_data_t* data, char* buf, int len, int command)
{
    OSStatus err = -1;
    int index = 0, version;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    MIDIObjectRef obj;
    AUGraph g;
    if (command == DRV_DISPOSE_AU_GRAPH) {
	if (!decode_au_graph(buf, &index, &g))
	    goto error;
    } else {
	if (!decode_midi_obj(buf, &index, &obj))
	    goto error;
    }
    if (command == DRV_DISPOSE_CLIENT)
	err = MIDIClientDispose(reinterpret_cast<MIDIClientRef>(obj));
    else if (command == DRV_DISPOSE_PORT)
	err = MIDIPortDispose(reinterpret_cast<MIDIPortRef>(obj));
    else if (command == DRV_DISPOSE_AU_GRAPH)
	err = DisposeAUGraph(g);
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}






