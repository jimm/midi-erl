/*
 * erl_midi_win.cpp
 *
 *  Created on: 2009-mar-07
 *      Author: Jakob
 */

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

#include <windows.h>
#include <mmsystem.h>

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
    DRV_LIST_MIDI_INPUTS = 1,
    DRV_LIST_MIDI_OUTPUTS,
    DRV_OPEN_MIDI_INPUT,
    DRV_OPEN_MIDI_OUTPUT,
    DRV_SEND_MIDI,
    DRV_NOW,
    DRV_TEST,
    DRV_CLOSE_MIDI_INPUT,
    DRV_CLOSE_MIDI_OUTPUT
};

static ErlDrvBinary* ei_x_to_new_binary(ei_x_buff* x)
{
    ErlDrvBinary* bin = driver_alloc_binary(x->index);
    if (bin != NULL)
	memcpy(&bin->orig_bytes[0], x->buff, x->index);
    return bin;
}

static void do_test(our_data_t* data);
static void do_list_midi_objects(our_data_t* data, int command);
static void do_open_midi_input(our_data_t* data, char* buf, int len);
static void do_open_midi_output(our_data_t* data, char* buf, int len);
static void do_send_midi(our_data_t* data, char* buf, int len);
static void do_now(our_data_t* data);
static void do_close_midi_input(our_data_t* data, char* buf, int len);
static void do_close_midi_output(our_data_t* data, char* buf, int len);

static int control(ErlDrvData drv_data, unsigned int command, char *buf,
	int len, char **rbuf, int rlen)
{
    int r = 0;
    DD("command %d", command);
    our_data_t* data = reinterpret_cast<our_data_t*> (drv_data);
    ei_x_new_with_version(&data->x);
    switch (command) {
    case DRV_TEST:
	do_test(data);
	break;

    case DRV_SET_SOUND_BANK:
    case DRV_DEVICE_LIST:
    case DRV_SOURCE_LIST:
    case DRV_DESTINATION_LIST:
	do_list_midi_objects(data, command);
	break;
    case DRV_OPEN_MIDI_INPUT:
	do_open_midi_input(data, buf, len);
	break;
    case DRV_OPEN_MIDI_OUTPUT:
	do_open_midi_output(data, buf, len);
	break;
    case DRV_SEND_MIDI:
	do_send_midi(data, buf, len);
	break;
    case DRV_NOW:
	do_now(data);
	break;
    case DRV_CLOSE_MIDI_INPUT:
	do_close_midi_input(data, buf, len);
	break;
    case DRV_CLOSE_MIDI_OUTPUT:
	do_close_midi_output(data, buf, len);
	break;
    default:
	r = -1;
	break;
    }
    if (r == 0) {
	*rbuf = reinterpret_cast<char*> (ei_x_to_new_binary(&data->x));
    }
    ei_x_free(&data->x);
    return r;
}

static void stop(ErlDrvData drv_data)
{
    //    do_disconnect((our_data_t*)drv_data);
}

static void do_test(our_data_t* data)
{
    ei_x_buff* x = &data->x;
    ei_x_encode_list_header(x, 2);
    ei_x_encode_atom(x, "ok");
    ei_x_encode_string(x, "test");
    ei_x_encode_empty_list(x);
}

//enum PutGet
//{
//    pgPut, pgGet
//};

static void encode_error(ei_x_buff* x, int err)
{
    ei_x_encode_tuple_header(x, 2);
    ei_x_encode_atom(x, "error");
    ei_x_encode_long(x, err);
}

static void encode_tupled_binary(ei_x_buff* x, const char* name, void* data, int size, int err)
{
    if (err == 0) {
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_atom(x, name);
	ei_x_encode_binary(x, data, size);
    } else
	encode_error(x, err);
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

static void do_list_midi_objects(our_data_t* data, int command)
{
    ei_x_buff* x = &data->x;
    int n = 0;
    switch (command) {
    case DRV_SOURCE_LIST:
	n = midiInGetNumDevs();
	break;
    case DRV_DESTINATION_LIST:
	n = midiOutGetNumDevs();
	break;
    }
    ei_x_encode_list_header(x, n);
    for (int i = 0; i < n; ++i) {
	MIDIINCAPSA mic;
	MIDIOUTCAPSA moc;
	const char* s;
	MMRESULT r;
	switch (command) {
	case DRV_DEVICE_LIST:
//	    obj = MIDIGetDevice(i);
	    break;
	case DRV_SOURCE_LIST:
	    r = midiInGetDevCaps(i, &mic, sizeof(mic));
	    if (r == MMSYSERR_NOERROR)
		s = mic.szPname;
	    else
		s = "error";
	    break;
	case DRV_DESTINATION_LIST:
	    r = midiOutGetDevCaps(i, &moc, sizeof(moc));
	    if (r == MMSYSERR_NOERROR)
		s = moc.szPname;
	    else
		s = "error";
	    break;
	}
	ei_x_encode_tuple_header(x, 2);
	ei_x_encode_ulong(x, i);
	ei_x_encode_string(x, s);
    }
    ei_x_encode_empty_list(x);
}

static void encode_ok_or_error(ei_x_buff* x, int err)
{
    if (err == 0)
	ei_x_encode_atom(x, "ok");
    else
	encode_error(x, err);
}


static void MyMidiInProc(
	  HMIDIIN hMidiIn,
	  UINT wMsg,
	  DWORD_PTR dwInstance,
	  DWORD_PTR dwParam1,
	  DWORD_PTR dwParam2);

static void do_open_midi_input(our_data_t* data, char* buf, int len)
{
    int err = -1;
    int index = 0, version;
    HMIDIIN handle = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    long id;
    if (ei_decode_long(buf, &index, &id) != 0)
	goto error;
    err = ::midiInOpen(&handle, id,
	    reinterpret_cast<DWORD_PTR>(MyMidiInProc),
	    reinterpret_cast<DWORD_PTR>(data), CALLBACK_FUNCTION);
    if (err != 0)
	goto error;
    err = ::midiInStart(id);
    error: ;
    ei_x_buff* x = &data->x;
    encode_tupled_binary(x, "midiin", &handle, sizeof(handle), err);
}

static void do_open_midi_output(our_data_t* data, char* buf, int len)
{
    int err = -1;
    int index = 0, version;
    HMIDIOUT handle = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    long id;
    if (ei_decode_long(buf, &index, &id) != 0)
	goto error;
    MMRESULT result = ::midiOutOpen(&handle, id, 0, 0, CALLBACK_NULL);
    if (result == MMSYSERR_NOERROR)
	err = 0;
    else
	err = result;
    error: ;
    ei_x_buff* x = &data->x;
    encode_tupled_binary(x, "midiout", &handle, sizeof(handle), err);
}

static void do_send_midi(our_data_t* data, char* buf, int len)
{
    int err = -1;
    int index = 0, version;
    HMIDIOUT handle = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    err = -3;
    int arity;
    if (ei_decode_tuple_header(buf, &index, &arity) != 0)
	goto error;
    err = -100-arity;
    if (arity != 2)
	goto error;
    err = -4;
    if (!decode_tupled_binary(buf, &index, "midiout", sizeof(handle), &handle))
	goto error;
    err = -5;
    int type, size;
    if (ei_get_type(buf, &index, &type, &size) != 0)
	goto error;
    err = -1000-type;
    if (type == ERL_INTEGER_EXT || ERL_SMALL_INTEGER_EXT) {
	err = -6;
	unsigned long l;
	if (ei_decode_ulong(buf, &index, &l) != 0)
	    goto error;
	err = midiOutShortMsg(handle, l);
	if (err != MMSYSERR_NOERROR)
	    goto error;
    } else if (type == ERL_BINARY_EXT) {
	char* p = NULL;
	try {
	    p = new char[size];
	    long n;
	    err = -7;
	    if (ei_decode_binary(buf, &index, p, &n) != 0)
		goto error;
	    MIDIHDR header;
	    header.lpData = p;
	    header.dwBufferLength = size;
	    header.dwFlags = 0;
	    err = midiOutPrepareHeader(handle, &header, sizeof(header));
	    if (err != MMSYSERR_NOERROR)
		goto error;
	    err = midiOutLongMsg(handle, &header, sizeof(header));
	    if (err != MMSYSERR_NOERROR)
		goto error;
	    delete[] p;
	} catch (...) {
	    delete[] p;
	}
    } else
	goto error;
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}

static void encode_now(ei_x_buff* x)
{
    MMTIME mmt;
    MMRESULT result = timeGetSystemTime(&mmt, sizeof(mmt));
    if (result == TIMERR_NOERROR)
	ei_x_encode_ulong(x, mmt.ms);
    else
	ei_x_encode_ulong(x, 0);
}

static void MyMidiInProc(
	  HMIDIIN hMidiIn,
	  UINT wMsg,
	  DWORD_PTR dwInstance,
	  DWORD_PTR dwParam1,
	  DWORD_PTR dwParam2)
{
    our_data_t* data = (our_data_t*) dwInstance;
    MIDIHDR* header = NULL;
    ei_x_buff x;
    ei_x_new_with_version(&x);
    ei_x_encode_tuple_header(&x, 4);
    switch (wMsg) {
    case MIM_DATA:      // Short message received
    case MIM_ERROR:     // Invalid short message received
	ei_x_encode_atom(&x, wMsg == MIM_DATA ? "midi_msg" : "midi_error");
	encode_now(&x);
	ei_x_encode_ulong(&x, dwParam1);
	break;
    case MIM_LONGDATA:  // System exclusive message received
    case MIM_LONGERROR: // Invalid system exclusive message received
	header = reinterpret_cast<MIDIHDR *>(dwParam1);
	ei_x_encode_atom(&x, wMsg == MIM_LONGDATA ? "midi_data" : "midi_error");
	encode_now(&x);
	ei_x_encode_binary(&x, header->lpData, header->dwBytesRecorded);
	break;
    }
    ei_x_encode_ulong(&x, dwParam2);
    driver_output(data->port, x.buff, x.index);
    ei_x_free(&x);
}

static void do_now(our_data_t* data)
{
    ei_x_buff* x = &data->x;
    encode_now(x);
}

static void do_close_midi_input(our_data_t* data, char* buf, int len)
 {
    int err = -1;
    int index = 0, version;
    HMIDIIN handle = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    if (!decode_tupled_binary(buf, &index, "midiin", sizeof(handle), &handle))
	goto error;
    err = ::midiInClose(&handle);
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}

static void do_close_midi_output(our_data_t* data, char* buf, int len)
 {
    int err = -1;
    int index = 0, version;
    HMIDIOUT handle = NULL;
    if (ei_decode_version(buf, &index, &version) != 0)
	goto error;
    if (!decode_tupled_binary(buf, &index, "midiout", sizeof(handle), &handle))
	goto error;
    err = ::midiOutClose(&handle);
    error: ;
    ei_x_buff* x = &data->x;
    encode_ok_or_error(x, err);
}




