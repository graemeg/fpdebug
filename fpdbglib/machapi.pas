unit machapi;
// mach_vm_size_t processor dependant!?

{$ifdef fpc}{$mode delphi}{$H+}{$endif}

interface


type
  boolean_t  = Integer;
//  integer_t  = Integer;
//  natural_t  = LongWord;
  uint64_t   = QWord;
  int64_t    = Int64;


{*
 *	Header file for basic, machine-dependent data types.  i386 version.
 *}

  int16_t = SmallInt;
  int32_t = Integer;
  uint32_t = LongWord;

  MACH_MSG_TYPE_REAL_32 = single;
  MACH_MSG_TYPE_REAL_64 = double;


// from ISO/IEC 988:1999 spec */
// 7.18.1.4 Integer types capable of hgolding object pointers */
{*
 * The [u]intptr_t types for the native
 * integer type, e.g. 32 or 64 or.. whatever
 * register size the machine has.  They are
 * used for entities that might be either
 * [unsigned] integers or pointers, and for
 * type-casting between the two.
 *
 * For instance, the IPC system represents
 * a port in user space as an integer and
 * in kernel space as a pointer.
 *}
//#if defined(__LP64__)
//type uintptr_t = uint64_t;
//type intptr_t = int64_t;
//#else
//type uintptr_t = uint32_t;
//type intptr_t = int32_t;
//#endif

{*
 * These are the legacy Mach types that are
 * the [rough] equivalents of the standards above.
 * They were defined in terms of int, not
 * long int, so they remain separate.
 *}
{if defined(__LP64__)
type register_t = int64_t;
#else
type register_t = int32_t;
#endif}
 register_t = int32_t;
 integer_t = int32_t;
 natural_t = uint32_t;

{*
 * These are the VM types that scale with the address
 * space size of a given process.
 *}

{if defined(__LP64__)
type vm_address_t = uint64_t;
type vm_offset_t = uint64_t;
type vm_size_t = uint64_t;
#else}
  vm_address_t = natural_t;
  vm_offset_t = natural_t;
  vm_size_t = natural_t;
//ndif

{*
 * The mach_vm_xxx_t types are sized to hold the
 * maximum pointer, offset, etc... supported on the
 * platform.
 *}
  mach_vm_address_t = uint64_t;
  mach_vm_offset_t = uint64_t;
  mach_vm_size_t = uint64_t;

{if	MACH_IPC_COMPAT
/*
 * For the old IPC interface
 */
#define	MSG_TYPE_PORT_NAME	natural_t

#endif	/* MACH_IPC_COMPAT */

#endif	/* _MACHINE_VM_TYPES_DEFS_ */

/* vim: set ft=c : *}


{	Time value returned by kernel.}
  time_value = record
  	seconds      : integer_t;
	  microseconds : integer_t;
  end;
  time_value_t = time_value;

// mach_types.h

type
  kern_return_t = Integer;

  mach_port_t = Integer;

  task_t   = mach_port_t;
  task_name_t = mach_port_t;
	thread_act_t = mach_port_t;
  ipc_space_t = mach_port_t		;
  thread_t = mach_port_t;
  host_t = mach_port_t;
  host_priv_t = mach_port_t;
  host_security_t = mach_port_t;
  processor_t = mach_port_t;
  processor_set_t = mach_port_t;
  processor_set_control_t = mach_port_t;
  semaphore_t = mach_port_t;
  lock_set_t = mach_port_t;
  ledger_t = mach_port_t;
  alarm_t = mach_port_t;
  clock_serv_t = mach_port_t;
  clock_ctrl_t = mach_port_t;

  mach_msg_type_number_t = Integer;

{ * Mig doesn't translate the components of an array.
  * For example, Mig won't use the thread_t translations
  * to translate a thread_array_t argument.  So, these definitions
  * are not completely accurate at the moment for other kernel
  * components. }

//typedef task_t			*task_array_t;
  Ttask_array = array [word] of task_t;
  Ptask_array = ^Ttask_array;
  //task_array_t = ^task_t;
  task_array_t = Ptask_array;
  //todo:
  thread_array_t = ^thread_t;
  processor_set_array_t = ^processor_set_t;
  processor_set_name_array_t = ^processor_set_t;
  processor_array_t  = ^processor_t;
  {Tthread_act_array_t = array [word] of thread_act_t;
  Pthread_act_array_t = ^Tthread_act_array_t;
  thread_act_array_t = Pthread_act_array_t;}
  thread_act_array_t = ^thread_act_t;


  ledger_array_t     = ^ledger_t;

{* These aren't really unique types.  They are just called
 * out as unique types at one point in history.  So we list
 * them here for compatibility. }
  processor_set_name_t =  processor_set_t;

  { These types are just hard-coded as ports }
  clock_reply_t = mach_port_t;
  bootstrap_t = mach_port_t;
  mem_entry_name_port_t =	mach_port_t;
  exception_handler_t = mach_port_t;

  Texception_handler_array = array [word] of exception_handler_t;
  Pexception_handler_array = ^Texception_handler_array;
  exception_handler_array_t = Pexception_handler_array;
  //exception_handler_array_t = ^exception_handler_t;

  vm_task_entry_t = mach_port_t;
  io_master_t = mach_port_t;
  UNDServerRef = mach_port_t;

const
  TASK_NULL	= 0;
  TASK_NAME_NULL = 0;
  THREAD_NULL		 = 0;
  THR_ACT_NULL 	 = 0;
  IPC_SPACE_NULL = 0;
  HOST_NULL		   = 0;
  HOST_PRIV_NULL = 0;
  HOST_SECURITY_NULL = 0;
  PROCESSOR_SET_NULL = 0;
  PROCESSOR_NULL	= 0;
  SEMAPHORE_NULL	= 0;
  LOCK_SET_NULL		= 0;
  LEDGER_NULL 		= 0;
  ALARM_NULL		  = 0;
  CLOCK_NULL		  = 0;
  UND_SERVER_NULL	= 0;

type
  ledger_item_t = natural_t;

  Temulation_vector = array [word] of mach_vm_offset_t;
  Pemulation_vector = ^Temulation_vector;
  //emulation_vector_t = ^mach_vm_offset_t;
  emulation_vector_t = Pemulation_vector;

  user_subsystem_t = Pchar;
  labelstr_t = Pchar;


// port.h

{*
 *	File:	mach/port.h
 *
 *	Definition of a Mach port
 *
 *	Mach ports are the endpoints to Mach-implemented communications
 *	channels (usually uni-directional message queues, but other types
 *	also exist).
 *
 *	Unique collections of these endpoints are maintained for each
 *	Mach task.  Each Mach port in the task's collection is given a
 *	[task-local] name to identify it - and the the various "rights"
 *	held by the task for that specific endpoint.
 *
 *	This header defines the types used to identify these Mach ports
 *	and the various rights associated with them.  For more info see:
 *
 *	<mach/mach_port.h> - manipulation of port rights in a given space
 *	<mach/message.h> - message queue [and port right passing] mechanism
 *
 *}


{*
*	mach_port_name_t - the local identity for a Mach port
*
*	The name is Mach port namespace specific.  It is used to
*	identify the rights held for that port by the task whose
*	namespace is implied [or specifically provided].
*
*	Use of this type usually implies just a name - no rights.
*	See mach_port_t for a type that implies a "named right."
*
*}

type
  mach_port_name_t = natural_t;
  mach_port_name_array_t = ^mach_port_name_t;

{*
 *	mach_port_t - a named port right
 *
 *	In user-space, "rights" are represented by the name of the
 *	right in the Mach port namespace.  Even so, this type is
 *	presented as a unique one to more clearly denote the presence
 *	of a right coming along with the name.
 *
 *	Often, various rights for a port held in a single name space
 *	will coalesce and are, therefore, be identified by a single name
 *	[this is the case for send and receive rights].  But not
 *	always [send-once rights currently get a unique name for
 *	each right].
 *
 *}
type
  mach_port_array_t = ^mach_port_t;

{*
 *  MACH_PORT_NULL is a legal value that can be carried in messages.
 *  It indicates the absence of any port or port rights.  (A port
 *  argument keeps the message from being "simple", even if the
 *  value is MACH_PORT_NULL.)  The value MACH_PORT_DEAD is also a legal
 *  value that can be carried in messages.  It indicates
 *  that a port right was present, but it died.
 *}

const
  MACH_PORT_NULL		= 0;   {* intentional loose typing *}
  MACH_PORT_DEAD		= mach_port_name_t($FFFFFFFF);
{  MACH_PORT_VALID(name)				\
		(((name) != MACH_PORT_NULL) && 		\
		 ((name) != MACH_PORT_DEAD))}


{*
 *  These are the different rights a task may have for a port.
 *  The MACH_PORT_RIGHT_* definitions are used as arguments
 *  to mach_port_allocate, mach_port_get_refs, etc, to specify
 *  a particular right to act upon.  The mach_port_names and
 *  mach_port_type calls return bitmasks using the MACH_PORT_TYPE_*
 *  definitions.  This is because a single name may denote
 *  multiple rights.
 *}

type
  mach_port_right_t =  natural_t;

const
  MACH_PORT_RIGHT_SEND		  = 0;
  MACH_PORT_RIGHT_RECEIVE		= 1;
  MACH_PORT_RIGHT_SEND_ONCE	= 2;
  MACH_PORT_RIGHT_PORT_SET	= 3;
  MACH_PORT_RIGHT_DEAD_NAME	= 4;
  MACH_PORT_RIGHT_LABELH	  = 5;
  MACH_PORT_RIGHT_NUMBER		= 6;

type
  mach_port_type_t = natural_t;
  mach_port_type_array_t = ^mach_port_type_t;

{#define MACH_PORT_TYPE(right)						\
  (
     (mach_port_type_t)
       (  1 << ((right) + (16)) }

const
  MACH_PORT_TYPE_NONE	      = 0;
  MACH_PORT_TYPE_SEND	      = 1 shl (MACH_PORT_RIGHT_SEND+16);
  MACH_PORT_TYPE_RECEIVE	  = 1 shl (MACH_PORT_RIGHT_RECEIVE+16);
  MACH_PORT_TYPE_SEND_ONCE  = 1 shl (MACH_PORT_RIGHT_SEND_ONCE+16);
  MACH_PORT_TYPE_PORT_SET	  = 1 shl (MACH_PORT_RIGHT_PORT_SET+16);
  MACH_PORT_TYPE_DEAD_NAME  = 1 shl (MACH_PORT_RIGHT_DEAD_NAME+16);
  MACH_PORT_TYPE_LABELH     = 1 shl (MACH_PORT_RIGHT_LABELH+16);

{ Convenient combinations. }

  MACH_PORT_TYPE_SEND_RECEIVE =	(MACH_PORT_TYPE_SEND or MACH_PORT_TYPE_RECEIVE);
  MACH_PORT_TYPE_SEND_RIGHTS  =	(MACH_PORT_TYPE_SEND or MACH_PORT_TYPE_SEND_ONCE);
  MACH_PORT_TYPE_PORT_RIGHTS	=	(MACH_PORT_TYPE_SEND_RIGHTS or MACH_PORT_TYPE_RECEIVE);
  MACH_PORT_TYPE_PORT_OR_DEAD =	(MACH_PORT_TYPE_PORT_RIGHTS or MACH_PORT_TYPE_DEAD_NAME);
  MACH_PORT_TYPE_ALL_RIGHTS	  =	(MACH_PORT_TYPE_PORT_OR_DEAD or MACH_PORT_TYPE_PORT_SET);

// Dummy type bits that mach_port_type/mach_port_names can return.

  MACH_PORT_TYPE_DNREQUEST	= $80000000;

// User-references for capabilities.
type
  mach_port_urefs_t = natural_t;
  mach_port_delta_t = integer_t;			{ change in urefs }

{ Attributes of ports.  (See mach_port_get_receive_status.) }

  mach_port_seqno_t = natural_t;		{ sequence number }
  mach_port_mscount_t = natural_t;		{ make-send count }
  mach_port_msgcount_t = natural_t;		{ number of msgs }
  mach_port_rights_t = natural_t;		{ number of rights }

{*
 *	Are there outstanding send rights for a given port?
 *}
const
	MACH_PORT_SRIGHTS_NONE		= 0;		{ no srights }
  MACH_PORT_SRIGHTS_PRESENT	= 1;		{ srights }

type
  mach_port_srights_t = LongWord;	{ status of send rights }

type
  mach_port_status = record
  	mps_pset      : mach_port_rights_t;	{ count of containing port sets }
	  mps_seqno     : mach_port_seqno_t	;	{ sequence number }
  	mps_mscount   : mach_port_mscount_t;	{ make-send count }
  	mps_qlimit    : mach_port_msgcount_t;	{ queue limit }
  	mps_msgcount  : mach_port_msgcount_t;	{ number in the queue }
  	mps_sorights  : mach_port_rights_t;	{ how many send-once rights }
  	mps_srights   : boolean_t;	{ do send rights exist? }
  	mps_pdrequest : boolean_t;	{ port-deleted requested? }
  	mps_nsrequest : boolean_t;	{ no-senders requested? }
  	mps_flags     : natural_t;		{ port flags }
  end;
  mach_port_status_t = mach_port_status;

{ System-wide values for setting queue limits on a port }
const
  MACH_PORT_QLIMIT_ZERO		 = 0;
  MACH_PORT_QLIMIT_BASIC	 = 5;
  MACH_PORT_QLIMIT_SMALL	 = 16;
  MACH_PORT_QLIMIT_LARGE 	 = 1024;
  MACH_PORT_QLIMIT_MIN		 = MACH_PORT_QLIMIT_ZERO;
  MACH_PORT_QLIMIT_DEFAULT = MACH_PORT_QLIMIT_BASIC;
  MACH_PORT_QLIMIT_MAX		 = MACH_PORT_QLIMIT_LARGE;

type
  mach_port_limits = record
  	mpl_qlimit: mach_port_msgcount_t;	{ number of msgs }
  end;
  mach_port_limits_t = mach_port_limits;

  mach_port_info_t = ^integer_t;		{ varying array of natural_t }

{ Flavors for mach_port_get/set_attributes() }
  mach_port_flavor_t = integer;

const
  MACH_PORT_LIMITS_INFO		  = 1; { uses mach_port_status_t }
  MACH_PORT_RECEIVE_STATUS	= 2; { uses mach_port_limits_t }
  MACH_PORT_DNREQUESTS_SIZE	= 3; { info is int }

  MACH_PORT_LIMITS_INFO_COUNT	= sizeof(mach_port_limits_t) div sizeof(natural_t);
  MACH_PORT_RECEIVE_STATUS_COUNT = sizeof(mach_port_status_t) div sizeof(natural_t);
  MACH_PORT_DNREQUESTS_SIZE_COUNT = 1;

{*
 * Structure used to pass information about port allocation requests.
 * Must be padded to 64-bits total length.
 *}
type
  mach_port_qos = record
    //unsigned int		name:1;		/* name given */
  	//unsigned int 		prealloc:1;	/* prealloced message */
  	//boolean_t		pad1:30;
    flags : LongWord;
   	len   : natural_t;
  end;
  mach_port_qos_t = mach_port_qos;

//#if	!__DARWIN_UNIX03 && !defined(_NO_PORT_T_FROM_MACH)
{*
 *  Mach 3.0 renamed everything to have mach_ in front of it.
 *  These types and macros are provided for backward compatibility
 *	but are deprecated.
 *}
type
  port_t = mach_port_t;
  port_name_t = mach_port_name_t	;
  port_name_array_t = ^mach_port_name_t;

const
  PORT_NULL	= 0;
  PORT_DEAD	= port_t($FFFFFFFF);
//#define PORT_VALID(name) \
//		((port_t)(name) != PORT_NULL && (port_t)(name) != PORT_DEAD)

{*******************************************************************************
 *	File:	mach/message.h                                                       *
 *                                                                             *
 *	Mach IPC message and primitive function definitions.                       *
 *******************************************************************************}


{*
 *  The timeout mechanism uses mach_msg_timeout_t values,
 *  passed by value.  The timeout units are milliseconds.
 *  It is controlled with the MACH_SEND_TIMEOUT
 *  and MACH_RCV_TIMEOUT options.
 *}

type
  mach_msg_timeout_t = natural_t ;

{*
 *  The value to be used when there is no timeout.
 *  (No MACH_SEND_TIMEOUT/MACH_RCV_TIMEOUT option.)
 *}
const
  MACH_MSG_TIMEOUT_NONE	= 0;

{*
 *  The kernel uses MACH_MSGH_BITS_COMPLEX as a hint.  It it isn't on, it
 *  assumes the body of the message doesn't contain port rights or OOL
 *  data.  The field is set in received messages.  A user task must
 *  use caution in interpreting the body of a message if the bit isn't
 *  on, because the mach_msg_type's in the body might "lie" about the
 *  contents.  If the bit isn't on, but the mach_msg_types
 *  in the body specify rights or OOL data, the behavior is undefined.
 *  (Ie, an error may or may not be produced.)
 *
 *  The value of MACH_MSGH_BITS_REMOTE determines the interpretation
 *  of the msgh_remote_port field.  It is handled like a msgt_name.
 *
 *  The value of MACH_MSGH_BITS_LOCAL determines the interpretation
 *  of the msgh_local_port field.  It is handled like a msgt_name.
 *
 *  MACH_MSGH_BITS() combines two MACH_MSG_TYPE_* values, for the remote
 *  and local fields, into a single value suitable for msgh_bits.
 *
 *  MACH_MSGH_BITS_CIRCULAR should be zero; is is used internally.
 *
 *  The unused bits should be zero and are reserved for the kernel
 *  or for future interface expansion.
 *}

const
  MACH_MSGH_BITS_ZERO	 	     = $00000000;
  MACH_MSGH_BITS_REMOTE_MASK = $000000ff;
  MACH_MSGH_BITS_LOCAL_MASK	 = $0000ff00;
  MACH_MSGH_BITS_COMPLEX		 = $80000000;
  MACH_MSGH_BITS_USER        = $8000ffff;

  MACH_MSGH_BITS_CIRCULAR	 	 = $40000000; { internal use only }
 	MACH_MSGH_BITS_USED		     = $c000ffff;

  MACH_MSGH_BITS_PORTS_MASK  = (MACH_MSGH_BITS_REMOTE_MASK or MACH_MSGH_BITS_LOCAL_MASK);

{#define MACH_MSGH_BITS(remote, local)				\
		((remote) | ((local) << 8))
#define	MACH_MSGH_BITS_REMOTE(bits)				\
		((bits) & MACH_MSGH_BITS_REMOTE_MASK)
#define	MACH_MSGH_BITS_LOCAL(bits)				\
		(((bits) & MACH_MSGH_BITS_LOCAL_MASK) >> 8)
#define	MACH_MSGH_BITS_PORTS(bits)				\
		((bits) & MACH_MSGH_BITS_PORTS_MASK)
#define	MACH_MSGH_BITS_OTHER(bits)				\
		((bits) &~ MACH_MSGH_BITS_PORTS_MASK)}

{*
 *  Every message starts with a message header.
 *  Following the message header are zero or more pairs of
 *  type descriptors (mach_msg_type_t/mach_msg_type_long_t) and
 *  data values.  The size of the message must be specified in bytes,
 *  and includes the message header, type descriptors, inline
 *  data, and inline pointer for out-of-line data.
 *
 *  The msgh_remote_port field specifies the destination of the message.
 *  It must specify a valid send or send-once right for a port.
 *
 *  The msgh_local_port field specifies a "reply port".  Normally,
 *  This field carries a send-once right that the receiver will use
 *  to reply to the message.  It may carry the values MACH_PORT_NULL,
 *  MACH_PORT_DEAD, a send-once right, or a send right.
 *
 *  The msgh_seqno field carries a sequence number associated with the
 *  received-from port.  A port's sequence number is incremented every
 *  time a message is received from it.  In sent messages, the field's
 *  value is ignored.
 *
 *  The msgh_id field is uninterpreted by the message primitives.
 *  It normally carries information specifying the format
 *  or meaning of the message.
 *}

type
  mach_msg_bits_t = LongWord;
  mach_msg_size_t =	natural_t;
  mach_msg_id_t = integer_t ;

const
  MACH_MSG_SIZE_NULL = 0; // (mach_msg_size_t *) 0

type
  mach_msg_type_name_t = LongWord;

const
  MACH_MSG_TYPE_MOVE_RECEIVE	 = 16; { Must hold receive rights }
  MACH_MSG_TYPE_MOVE_SEND		   = 17; { Must hold send rights }
  MACH_MSG_TYPE_MOVE_SEND_ONCE = 18; { Must hold sendonce rights }
  MACH_MSG_TYPE_COPY_SEND		   = 19; { Must hold send rights }
  MACH_MSG_TYPE_MAKE_SEND		   = 20; { Must hold receive rights }
  MACH_MSG_TYPE_MAKE_SEND_ONCE = 21; { Must hold receive rights }
  MACH_MSG_TYPE_COPY_RECEIVE	 = 22; { Must hold receive rights }

type
  mach_msg_copy_options_t = LongWord;

const
  MACH_MSG_PHYSICAL_COPY = 0;
  MACH_MSG_VIRTUAL_COPY = 1;
  MACH_MSG_ALLOCATE = 2;
  MACH_MSG_OVERWRITE = 3;
  MACH_MSG_KALLOC_COPY_T = 4;

type
  mach_msg_descriptor_type_t = LongWord;

const
  MACH_MSG_PORT_DESCRIPTOR = 0;
  MACH_MSG_OOL_DESCRIPTOR  = 1;
  MACH_MSG_OOL_PORTS_DESCRIPTOR =	2;
  MACH_MSG_OOL_VOLATILE_DESCRIPTOR = 3;

//#pragma pack(4)

(*

typedef struct
{
  natural_t			pad1;
  mach_msg_size_t		pad2;
  unsigned int			pad3 : 24;
  mach_msg_descriptor_type_t	type : 8;
} mach_msg_type_descriptor_t;

typedef struct
{
  mach_port_t			name;
  mach_msg_size_t		pad1;
  unsigned int			pad2 : 16;
  mach_msg_type_name_t		disposition : 8;
  mach_msg_descriptor_type_t	type : 8;
} mach_msg_port_descriptor_t;

typedef struct
{
  uint32_t			address;
  mach_msg_size_t       	size;
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  unsigned int     		pad1: 8;
  mach_msg_descriptor_type_t    type: 8;
} mach_msg_ool_descriptor32_t;

typedef struct
{
  uint64_t			address;
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  unsigned int     		pad1: 8;
  mach_msg_descriptor_type_t    type: 8;
  mach_msg_size_t       	size;
} mach_msg_ool_descriptor64_t;

typedef struct
{
  void*				address;
#if !defined(__LP64__)
  mach_msg_size_t       	size;
#endif
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  unsigned int     		pad1: 8;
  mach_msg_descriptor_type_t    type: 8;
#if defined(__LP64__)
  mach_msg_size_t       	size;
#endif
} mach_msg_ool_descriptor_t;

typedef struct
{
  uint32_t			address;
  mach_msg_size_t		count;
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  mach_msg_type_name_t		disposition : 8;
  mach_msg_descriptor_type_t	type : 8;
} mach_msg_ool_ports_descriptor32_t;

typedef struct
{
  uint64_t			address;
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  mach_msg_type_name_t		disposition : 8;
  mach_msg_descriptor_type_t	type : 8;
  mach_msg_size_t		count;
} mach_msg_ool_ports_descriptor64_t;

typedef struct
{
  void*				address;
#if !defined(__LP64__)
  mach_msg_size_t		count;
#endif
  boolean_t     		deallocate: 8;
  mach_msg_copy_options_t       copy: 8;
  mach_msg_type_name_t		disposition : 8;
  mach_msg_descriptor_type_t	type : 8;
#if defined(__LP64__)
  mach_msg_size_t		count;
#endif
} mach_msg_ool_ports_descriptor_t;

/*
 * LP64support - This union definition is not really
 * appropriate in LP64 mode because not all descriptors
 * are of the same size in that environment.
 */
typedef union
{
  mach_msg_port_descriptor_t		port;
  mach_msg_ool_descriptor_t		out_of_line;
  mach_msg_ool_ports_descriptor_t	ool_ports;
  mach_msg_type_descriptor_t		type;
} mach_msg_descriptor_t;

typedef struct
{
        mach_msg_size_t msgh_descriptor_count;
} mach_msg_body_t;

#define MACH_MSG_BODY_NULL (mach_msg_body_t * ) 0
#define MACH_MSG_DESCRIPTOR_NULL (mach_msg_descriptor_t * ) 0

typedef	struct
{
  mach_msg_bits_t	msgh_bits;
  mach_msg_size_t	msgh_size;
  mach_port_t		msgh_remote_port;
  mach_port_t		msgh_local_port;
  mach_msg_size_t 	msgh_reserved;
  mach_msg_id_t		msgh_id;
} mach_msg_header_t;

#define MACH_MSG_NULL (mach_msg_header_t * ) 0

typedef struct
{
        mach_msg_header_t       header;
        mach_msg_body_t         body;
} mach_msg_base_t;

typedef	unsigned int mach_msg_trailer_type_t;

#define	MACH_MSG_TRAILER_FORMAT_0	0

typedef	unsigned int mach_msg_trailer_size_t;

typedef struct
{
  mach_msg_trailer_type_t	msgh_trailer_type;
  mach_msg_trailer_size_t	msgh_trailer_size;
} mach_msg_trailer_t;

typedef struct
{
  mach_msg_trailer_type_t       msgh_trailer_type;
  mach_msg_trailer_size_t       msgh_trailer_size;
  mach_port_seqno_t             msgh_seqno;
} mach_msg_seqno_trailer_t;

typedef struct
{
  unsigned int			val[2];
} security_token_t;

typedef struct
{
  mach_msg_trailer_type_t	msgh_trailer_type;
  mach_msg_trailer_size_t	msgh_trailer_size;
  mach_port_seqno_t		msgh_seqno;
  security_token_t		msgh_sender;
} mach_msg_security_trailer_t;

/*
 * The audit token is an opaque token which identifies
 * Mach tasks and senders of Mach messages as subjects
 * to the BSM audit system.  Only the appropriate BSM
 * library routines should be used to interpret the
 * contents of the audit token as the representation
 * of the subject identity within the token may change
 * over time.
 */
typedef struct
{
  unsigned int			val[8];
} audit_token_t;

typedef struct
{
  mach_msg_trailer_type_t	msgh_trailer_type;
  mach_msg_trailer_size_t	msgh_trailer_size;
  mach_port_seqno_t		msgh_seqno;
  security_token_t		msgh_sender;
  audit_token_t			msgh_audit;
} mach_msg_audit_trailer_t;

typedef struct
{
  mach_port_name_t sender;
} msg_labels_t;

/*
   Trailer type to pass MAC policy label info as a mach message trailer.

*/

typedef struct
{
  mach_msg_trailer_type_t       msgh_trailer_type;
  mach_msg_trailer_size_t       msgh_trailer_size;
  mach_port_seqno_t             msgh_seqno;
  security_token_t              msgh_sender;
  audit_token_t			msgh_audit;
  msg_labels_t                  msgh_labels;
  int				msgh_ad;
} mach_msg_mac_trailer_t;

#define MACH_MSG_TRAILER_MINIMUM_SIZE  sizeof(mach_msg_trailer_t)

/*
 * These values can change from release to release - but clearly
 * code cannot request additional trailer elements one was not
 * compiled to understand.  Therefore, it is safe to use this
 * constant when the same module specified the receive options.
 * Otherwise, you run the risk that the options requested by
 * another module may exceed the local modules notion of
 * MAX_TRAILER_SIZE.
 */
typedef mach_msg_mac_trailer_t mach_msg_max_trailer_t;
#define MAX_TRAILER_SIZE sizeof(mach_msg_max_trailer_t)

/*
 * Legacy requirements keep us from ever updating these defines (even
 * when the format_0 trailers gain new option data fields in the future).
 * Therefore, they shouldn't be used going forward.  Instead, the sizes
 * should be compared against the specific element size requested using
 * REQUESTED_TRAILER_SIZE.
 */
typedef mach_msg_security_trailer_t mach_msg_format_0_trailer_t;

/*typedef mach_msg_mac_trailer_t mach_msg_format_0_trailer_t;
*/

#define MACH_MSG_TRAILER_FORMAT_0_SIZE sizeof(mach_msg_format_0_trailer_t)

#define   KERNEL_SECURITY_TOKEN_VALUE  { {0, 1} }
extern security_token_t KERNEL_SECURITY_TOKEN;

#define   KERNEL_AUDIT_TOKEN_VALUE  { {0, 0, 0, 0, 0, 0, 0, 0} }
extern audit_token_t KERNEL_AUDIT_TOKEN;

typedef	integer_t mach_msg_options_t;

typedef struct
{
  mach_msg_header_t	header;
} mach_msg_empty_send_t;

typedef struct
{
  mach_msg_header_t	header;
  mach_msg_trailer_t	trailer;
} mach_msg_empty_rcv_t;

typedef union
{
  mach_msg_empty_send_t	send;
  mach_msg_empty_rcv_t	rcv;
} mach_msg_empty_t;

#pragma pack()

/* utility to round the message size - will become machine dependent */
#define round_msg(x)	(((mach_msg_size_t)(x) + sizeof (natural_t) - 1) & \
				~(sizeof (natural_t) - 1))

/*
 *  There is no fixed upper bound to the size of Mach messages.
 */

#define	MACH_MSG_SIZE_MAX	((mach_msg_size_t) ~0)

/*
 *  Compatibility definitions, for code written
 *  when there was a msgh_kind instead of msgh_seqno.
 */
#define MACH_MSGH_KIND_NORMAL		0x00000000
#define MACH_MSGH_KIND_NOTIFICATION	0x00000001
#define	msgh_kind			msgh_seqno
#define mach_msg_kind_t			mach_port_seqno_t

/*
 *  The msgt_number field specifies the number of data elements.
 *  The msgt_size field specifies the size of each data element, in bits.
 *  The msgt_name field specifies the type of each data element.
 *  If msgt_inline is TRUE, the data follows the type descriptor
 *  in the body of the message.  If msgt_inline is FALSE, then a pointer
 *  to the data should follow the type descriptor, and the data is
 *  sent out-of-line.  In this case, if msgt_deallocate is TRUE,
 *  then the out-of-line data is moved (instead of copied) into the message.
 *  If msgt_longform is TRUE, then the type descriptor is actually
 *  a mach_msg_type_long_t.
 *
 *  The actual amount of inline data following the descriptor must
 *  a multiple of the word size.  For out-of-line data, this is a
 *  pointer.  For inline data, the supplied data size (calculated
 *  from msgt_number/msgt_size) is rounded up.  This guarantees
 *  that type descriptors always fall on word boundaries.
 *
 *  For port rights, msgt_size must be 8*sizeof(mach_port_t).
 *  If the data is inline, msgt_deallocate should be FALSE.
 *  The msgt_unused bit should be zero.
 *  The msgt_name, msgt_size, msgt_number fields in
 *  a mach_msg_type_long_t should be zero.
 */

typedef natural_t mach_msg_type_size_t;
typedef natural_t mach_msg_type_number_t;

/*
 *  Values received/carried in messages.  Tells the receiver what
 *  sort of port right he now has.
 *
 *  MACH_MSG_TYPE_PORT_NAME is used to transfer a port name
 *  which should remain uninterpreted by the kernel.  (Port rights
 *  are not transferred, just the port name.)
 */

#define MACH_MSG_TYPE_PORT_NONE		0

#define MACH_MSG_TYPE_PORT_NAME		15
#define MACH_MSG_TYPE_PORT_RECEIVE	MACH_MSG_TYPE_MOVE_RECEIVE
#define MACH_MSG_TYPE_PORT_SEND		MACH_MSG_TYPE_MOVE_SEND
#define MACH_MSG_TYPE_PORT_SEND_ONCE	MACH_MSG_TYPE_MOVE_SEND_ONCE

#define MACH_MSG_TYPE_LAST		22		/* Last assigned */

/*
 *  A dummy value.  Mostly used to indicate that the actual value
 *  will be filled in later, dynamically.
 */

#define MACH_MSG_TYPE_POLYMORPHIC	((mach_msg_type_name_t) -1)

/*
 *	Is a given item a port type?
 */

#define MACH_MSG_TYPE_PORT_ANY(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
	 ((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))

#define	MACH_MSG_TYPE_PORT_ANY_SEND(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_SEND) &&		\
	 ((x) <= MACH_MSG_TYPE_MAKE_SEND_ONCE))

#define	MACH_MSG_TYPE_PORT_ANY_RIGHT(x)			\
	(((x) >= MACH_MSG_TYPE_MOVE_RECEIVE) &&		\
	 ((x) <= MACH_MSG_TYPE_MOVE_SEND_ONCE))

typedef integer_t mach_msg_option_t;

#define MACH_MSG_OPTION_NONE	0x00000000

#define	MACH_SEND_MSG		0x00000001
#define	MACH_RCV_MSG		0x00000002
#define MACH_RCV_LARGE		0x00000004

#define MACH_SEND_TIMEOUT	0x00000010
#define MACH_SEND_INTERRUPT	0x00000040	/* libmach implements */
#define MACH_SEND_CANCEL	0x00000080
#define MACH_SEND_ALWAYS	0x00010000	/* internal use only */
#define MACH_SEND_TRAILER	0x00020000

#define MACH_RCV_TIMEOUT	0x00000100
#define MACH_RCV_NOTIFY		0x00000200
#define MACH_RCV_INTERRUPT	0x00000400	/* libmach implements */
#define MACH_RCV_OVERWRITE	0x00001000

/*
 * NOTE: a 0x00------ RCV mask implies to ask for
 * a MACH_MSG_TRAILER_FORMAT_0 with 0 Elements,
 * which is equivalent to a mach_msg_trailer_t.
 *
 * XXXMAC: unlike the rest of the MACH_RCV_* flags, MACH_RCV_TRAILER_LABELS
 * and MACH_RCV_TRAILER_AV need their own private bit since we only calculate
 * their fields when absolutely required.  This will cause us problems if
 * Apple adds new trailers.
 */
#define MACH_RCV_TRAILER_NULL   0
#define MACH_RCV_TRAILER_SEQNO  1
#define MACH_RCV_TRAILER_SENDER 2
#define MACH_RCV_TRAILER_AUDIT  3
#define MACH_RCV_TRAILER_LABELS 4
#define MACH_RCV_TRAILER_AV     8

#define MACH_RCV_TRAILER_TYPE(x)     (((x) & 0xf) << 28)
#define MACH_RCV_TRAILER_ELEMENTS(x) (((x) & 0xf) << 24)
#define MACH_RCV_TRAILER_MASK 	     ((0xff << 24))

#define GET_RCV_ELEMENTS(y) (((y) >> 24) & 0xf)

/*
 * XXXMAC: note that in the case of MACH_RCV_TRAILER_AV and
 * MACH_RCV_TRAILER_LABELS, we just fall through to mach_msg_max_trailer_t.
 * This is correct behavior since mach_msg_max_trailer_t is defined as
 * mac_msg_mac_trailer_t which is used for the LABELS and AV trailers.
 * It also makes things work properly if MACH_RCV_TRAILER_AV or
 * MACH_RCV_TRAILER_LABELS are ORed with one of the other options.
 */
#define REQUESTED_TRAILER_SIZE(y) 				\
	((mach_msg_trailer_size_t)				\
	 ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_NULL) ?	\
	  sizeof(mach_msg_trailer_t) :				\
	  ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_SEQNO) ?	\
	   sizeof(mach_msg_seqno_trailer_t) :			\
	  ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_SENDER) ?	\
	   sizeof(mach_msg_security_trailer_t) :		\
	   ((GET_RCV_ELEMENTS(y) == MACH_RCV_TRAILER_AUDIT) ?	\
	    sizeof(mach_msg_audit_trailer_t) :      		\
	    sizeof(mach_msg_max_trailer_t))))))

/*
 *  Much code assumes that mach_msg_return_t == kern_return_t.
 *  This definition is useful for descriptive purposes.
 *
 *  See <mach/error.h> for the format of error codes.
 *  IPC errors are system 4.  Send errors are subsystem 0;
 *  receive errors are subsystem 1.  The code field is always non-zero.
 *  The high bits of the code field communicate extra information
 *  for some error codes.  MACH_MSG_MASK masks off these special bits.
 */

typedef kern_return_t mach_msg_return_t;

#define MACH_MSG_SUCCESS		0x00000000


#define	MACH_MSG_MASK			0x00003e00
		/* All special error code bits defined below. */
#define	MACH_MSG_IPC_SPACE		0x00002000
		/* No room in IPC name space for another capability name. */
#define	MACH_MSG_VM_SPACE		0x00001000
		/* No room in VM address space for out-of-line memory. */
#define	MACH_MSG_IPC_KERNEL		0x00000800
		/* Kernel resource shortage handling an IPC capability. */
#define	MACH_MSG_VM_KERNEL		0x00000400
		/* Kernel resource shortage handling out-of-line memory. */

#define MACH_SEND_IN_PROGRESS		0x10000001
		/* Thread is waiting to send.  (Internal use only.) */
#define MACH_SEND_INVALID_DATA		0x10000002
		/* Bogus in-line data. */
#define MACH_SEND_INVALID_DEST		0x10000003
		/* Bogus destination port. */
#define MACH_SEND_TIMED_OUT		0x10000004
		/* Message not sent before timeout expired. */
#define MACH_SEND_INTERRUPTED		0x10000007
		/* Software interrupt. */
#define MACH_SEND_MSG_TOO_SMALL		0x10000008
		/* Data doesn't contain a complete message. */
#define MACH_SEND_INVALID_REPLY		0x10000009
		/* Bogus reply port. */
#define MACH_SEND_INVALID_RIGHT		0x1000000a
		/* Bogus port rights in the message body. */
#define MACH_SEND_INVALID_NOTIFY	0x1000000b
		/* Bogus notify port argument. */
#define MACH_SEND_INVALID_MEMORY	0x1000000c
		/* Invalid out-of-line memory pointer. */
#define MACH_SEND_NO_BUFFER		0x1000000d
		/* No message buffer is available. */
#define MACH_SEND_TOO_LARGE		0x1000000e
		/* Send is too large for port */
#define MACH_SEND_INVALID_TYPE		0x1000000f
		/* Invalid msg-type specification. */
#define MACH_SEND_INVALID_HEADER	0x10000010
		/* A field in the header had a bad value. */
#define MACH_SEND_INVALID_TRAILER	0x10000011
		/* The trailer to be sent does not match kernel format. */
#define MACH_SEND_INVALID_RT_OOL_SIZE	0x10000015
		/* compatibility: no longer a returned error */

#define MACH_RCV_IN_PROGRESS		0x10004001
		/* Thread is waiting for receive.  (Internal use only.) */
#define MACH_RCV_INVALID_NAME		0x10004002
		/* Bogus name for receive port/port-set. */
#define MACH_RCV_TIMED_OUT		0x10004003
		/* Didn't get a message within the timeout value. */
#define MACH_RCV_TOO_LARGE		0x10004004
		/* Message buffer is not large enough for inline data. */
#define MACH_RCV_INTERRUPTED		0x10004005
		/* Software interrupt. */
#define MACH_RCV_PORT_CHANGED		0x10004006
		/* compatibility: no longer a returned error */
#define MACH_RCV_INVALID_NOTIFY		0x10004007
		/* Bogus notify port argument. */
#define MACH_RCV_INVALID_DATA		0x10004008
		/* Bogus message buffer for inline data. */
#define MACH_RCV_PORT_DIED		0x10004009
		/* Port/set was sent away/died during receive. */
#define	MACH_RCV_IN_SET			0x1000400a
		/* compatibility: no longer a returned error */
#define	MACH_RCV_HEADER_ERROR		0x1000400b
		/* Error receiving message header.  See special bits. */
#define	MACH_RCV_BODY_ERROR		0x1000400c
		/* Error receiving message body.  See special bits. */
#define	MACH_RCV_INVALID_TYPE		0x1000400d
		/* Invalid msg-type specification in scatter list. */
#define	MACH_RCV_SCATTER_SMALL		0x1000400e
		/* Out-of-line overwrite region is not large enough */
#define MACH_RCV_INVALID_TRAILER	0x1000400f
		/* trailer type or number of trailer elements not supported */
#define MACH_RCV_IN_PROGRESS_TIMED      0x10004011
                /* Waiting for receive with timeout. (Internal use only.) */


__BEGIN_DECLS

/*
 *	Routine:	mach_msg_overwrite
 *	Purpose:
 *		Send and/or receive a message.  If the message operation
 *		is interrupted, and the user did not request an indication
 *		of that fact, then restart the appropriate parts of the
 *		operation silently (trap version does not restart).
 *
 *		Distinct send and receive buffers may be specified.  If
 *		no separate receive buffer is specified, the msg parameter
 *		will be used for both send and receive operations.
 *
 *		In addition to a distinct receive buffer, that buffer may
 *		already contain scatter control information to direct the
 *		receiving of the message.
 */

extern mach_msg_return_t	mach_msg_overwrite(
					mach_msg_header_t *msg,
					mach_msg_option_t option,
					mach_msg_size_t send_size,
					mach_msg_size_t rcv_size,
					mach_port_name_t rcv_name,
					mach_msg_timeout_t timeout,
					mach_port_name_t notify,
					mach_msg_header_t *rcv_msg,
					mach_msg_size_t rcv_limit);


/*
 *	Routine:	mach_msg
 *	Purpose:
 *		Send and/or receive a message.  If the message operation
 *		is interrupted, and the user did not request an indication
 *		of that fact, then restart the appropriate parts of the
 *		operation silently (trap version does not restart).
 */
extern mach_msg_return_t	mach_msg(
					mach_msg_header_t *msg,
					mach_msg_option_t option,
					mach_msg_size_t send_size,
					mach_msg_size_t rcv_size,
					mach_port_name_t rcv_name,
					mach_msg_timeout_t timeout,
					mach_port_name_t notify);


__END_DECLS

#endif	/* _MACH_MESSAGE_H_ */   *)


// ----- policy.h --------------------------------------------------------------

// *	Definitions for scheduing policy.

{*
 *  All interfaces defined here are obsolete.
 *}
type
  policy_t = integer;
  policy_info_t = ^integer_t;
  policy_base_t = ^integer_t;
  policy_limit_t = ^integer_t;

{*
 *	Policy definitions.  Policies should be powers of 2,
 *	but cannot be or'd together other than to test for a
 *	policy 'class'.
 *}
const
	POLICY_NULL		   = 0;	{* none			*}
 	POLICY_TIMESHARE = 1;	{* timesharing		*}
	POLICY_RR		  = 2;	{* fixed round robin	*}
  POLICY_FIFO		= 4;	{* fixed fifo		*}

  POLICYCLASS_FIXEDPRI=	(POLICY_RR or POLICY_FIFO) ;
{
/*
 *	Check if policy is valid.
 */
#define invalid_policy(policy)			\
	((policy) != POLICY_TIMESHARE &&	\
	 (policy) != POLICY_RR &&		\
	 (policy) != POLICY_FIFO)
}

{*
 * 	Types for TIMESHARE policy
 *}
type
  policy_timeshare_base = record
  	base_priority: integer_t		;
  end;

  policy_timeshare_limit = record
  	max_priority : integer_t;
  end;

  policy_timeshare_info = record
  	max_priority     : integer_t;
	  base_priority    : integer_t;
	  cur_priority     : integer_t;
  	depressed        : boolean_t;
  	depress_priority : integer_t;
  end;

  policy_timeshare_base_t = ^policy_timeshare_base;
  policy_timeshare_limit_t = ^policy_timeshare_limit;
  policy_timeshare_info_t = ^policy_timeshare_info;

  policy_timeshare_base_data_t = policy_timeshare_base;
  policy_timeshare_limit_data_t = policy_timeshare_limit;
  policy_timeshare_info_data_t = policy_timeshare_info;

const
  POLICY_TIMESHARE_BASE_COUNT  = sizeof(policy_timeshare_base) div sizeof(integer_t);
  POLICY_TIMESHARE_LIMIT_COUNT = sizeof(policy_timeshare_limit) div sizeof(integer_t);
  POLICY_TIMESHARE_INFO_COUNT	 = sizeof(policy_timeshare_info) div sizeof(integer_t);

{*
 *	Types for the ROUND ROBIN (RR) policy
 *}

type
  policy_rr_base = record
  	base_priority : integer_t;
    quantum       :	integer_t;
  end;

  policy_rr_limit = record
  	max_priority  : integer_t;
  end;

  policy_rr_info = record
  	max_priority  : integer_t;
	  base_priority : integer_t;
  	quantum       : integer_t;
	  depressed     : boolean_t;
	  depress_priority : integer_t;
  end;

  policy_rr_base_t = ^policy_rr_base;
  policy_rr_limit_t = ^policy_rr_limit;
  policy_rr_info_t = ^policy_rr_info;

  policy_rr_base_data_t = policy_rr_base;
  policy_rr_limit_data_t = policy_rr_limit;
  policy_rr_info_data_t = policy_rr_info;

const
  POLICY_RR_BASE_COUNT = sizeof(policy_rr_base) div sizeof(integer_t);
  POLICY_RR_LIMIT_COUNT	= sizeof(policy_rr_limit) div sizeof(integer_t);
  POLICY_RR_INFO_COUNT= sizeof(policy_rr_info) div sizeof(integer_t);

{*
 * 	Types for the FIRST-IN-FIRST-OUT (FIFO) policy
 *}

type
  policy_fifo_base = record
  	base_priority: integer_t;
  end;

  policy_fifo_limit = record
	  max_priority : integer_t;
  end;

  policy_fifo_info = record
    max_priority  : integer_t;
 	  base_priority : integer_t;
    depressed     : boolean_t;
 	  depress_priority  : integer_t;
  end;

  policy_fifo_base_t = ^policy_fifo_base;
  policy_fifo_limit_t = ^policy_fifo_limit;
  policy_fifo_info_t = ^policy_fifo_info;

  policy_fifo_base_data_t = policy_fifo_base;
  policy_fifo_limit_data_t = policy_fifo_limit	;
  policy_fifo_info_data_t = policy_fifo_info;

const
	POLICY_FIFO_BASE_COUNT = sizeof(policy_fifo_base) div sizeof(integer_t);
  POLICY_FIFO_LIMIT_COUNT	= sizeof(policy_fifo_limit) div sizeof(integer_t);
  POLICY_FIFO_INFO_COUNT = sizeof(policy_fifo_info) div sizeof(integer_t);


{*
 * 	Aggregate policy types
 *}

type
  policy_bases = record
  	ts : policy_timeshare_base_data_t;
   	rr : policy_rr_base_data_t;
  	fifo : policy_fifo_base_data_t;
  end;

  policy_limits = record
  	ts: policy_timeshare_limit_data_t;
	  rr: policy_rr_limit_data_t;
  	fifo: policy_fifo_limit_data_t;
  end;

  policy_infos = record
  	ts: policy_timeshare_info_data_t;
  	rr: policy_rr_info_data_t;
    fifo: policy_fifo_info_data_t;
  end;

  policy_base_data_t = policy_bases;
  policy_limit_data_t = policy_limits;
  policy_info_data_t = policy_infos;


// thread_policy.h

{*
 * These are the calls for accessing the policy parameters
 * of a particular thread.
 *
 * The extra 'get_default' parameter to the second call is
 * IN/OUT as follows:
 * 1) if asserted on the way in it indicates that the default
 * values should be returned, not the ones currently set, in
 * this case 'get_default' will always be asserted on return;
 * 2) if unasserted on the way in, the current settings are
 * desired and if still unasserted on return, then the info
 * returned reflects the current settings, otherwise if
 * 'get_default' returns asserted, it means that there are no
 * current settings due to other parameters taking precedence,
 * and the default ones are being returned instead.
 *}

type
  thread_policy_flavor_t = natural_t;
  thread_policy_t = ^integer_t;

{*
 * Defined flavors.
 *}
{*
 * THREAD_STANDARD_POLICY:
 *
 * This is the standard (fair) scheduling mode, assigned to new
 * threads.  The thread will be given processor time in a manner
 * which apportions approximately equal share to long running
 * computations.
 *
 * Parameters:
 *	[none]
 *}

const
  THREAD_STANDARD_POLICY		= 1;

type
  _thread_standard_policy = record
  	no_data: natural_t;
  end;

  thread_standard_policy_data_t = _thread_standard_policy;
  thread_standard_policy_t = ^_thread_standard_policy;

const
  THREAD_STANDARD_POLICY_COUNT	= 0;

{*
 * THREAD_EXTENDED_POLICY:
 *
 * Extended form of THREAD_STANDARD_POLICY, which supplies a
 * hint indicating whether this is a long running computation.
 *
 * Parameters:
 *
 * timeshare: TRUE (the default) results in identical scheduling
 * behavior as THREAD_STANDARD_POLICY.
 *}

const
  THREAD_EXTENDED_POLICY			= 1;

type
  _thread_extended_policy = record
  	timeshare: boolean_t;
  end;

  thread_extended_policy_data_t = _thread_extended_policy;
  thread_extended_policy_t = ^_thread_extended_policy;

const
  THREAD_EXTENDED_POLICY_COUNT = sizeof (thread_extended_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_TIME_CONSTRAINT_POLICY:
 *
 * This scheduling mode is for threads which have real time
 * constraints on their execution.
 *
 * Parameters:
 *
 * period: This is the nominal amount of time between separate
 * processing arrivals, specified in absolute time units.  A
 * value of 0 indicates that there is no inherent periodicity in
 * the computation.
 *
 * computation: This is the nominal amount of computation
 * time needed during a separate processing arrival, specified
 * in absolute time units.
 *
 * constraint: This is the maximum amount of real time that
 * may elapse from the start of a separate processing arrival
 * to the end of computation for logically correct functioning,
 * specified in absolute time units.  Must be (>= computation).
 * Note that latency = (constraint - computation).
 *
 * preemptible: This indicates that the computation may be
 * interrupted, subject to the constraint specified above.
 *}

const
  THREAD_TIME_CONSTRAINT_POLICY	= 2;

type
  _thread_time_constraint_policy = record
  	period      : uint32_t;
  	computation : uint32_t;
   	constraint  : uint32_t;
  	preemptible : boolean_t;
  end;

  thread_time_constraint_policy_data_t = _thread_time_constraint_policy;
  thread_time_constraint_policy_t = ^_thread_time_constraint_policy;

const
  THREAD_TIME_CONSTRAINT_POLICY_COUNT =
	  sizeof (thread_time_constraint_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_PRECEDENCE_POLICY:
 *
 * This may be used to indicate the relative value of the
 * computation compared to the other threads in the task.
 *
 * Parameters:
 *
 * importance: The importance is specified as a signed value.
 *}

const
 THREAD_PRECEDENCE_POLICY		= 3;

type
  _thread_precedence_policy = record
  	importance : integer_t;
  end;

  thread_precedence_policy_data_t = _thread_precedence_policy;
  thread_precedence_policy_t = ^_thread_precedence_policy;

const
  THREAD_PRECEDENCE_POLICY_COUNT =sizeof (thread_precedence_policy_data_t) div sizeof (integer_t);

{*
 * THREAD_AFFINITY_POLICY:
 *
 * This policy is experimental.
 * This may be used to express affinity relationships
 * between threads in the task. Threads with the same affinity tag will
 * be scheduled to share an L2 cache if possible. That is, affinity tags
 * are a hint to the scheduler for thread placement.
 *
 * The namespace of affinity tags is generally local to one task. However,
 * a child task created after the assignment of affinity tags by its parent
 * will share that namespace. In particular, a family of forked processes
 * may be created with a shared affinity namespace.
 *
 * Parameters:
 * tag: The affinity set identifier.
 *}

const
  THREAD_AFFINITY_POLICY		= 4;

type
  _thread_affinity_policy = record
  	affinity_tag: integer_t	;
  end;

type
  thread_affinity_policy_data_t = _thread_affinity_policy;
  thread_affinity_policy_t = ^_thread_affinity_policy;

const
  THREAD_AFFINITY_TAG_NULL		= 0;
  THREAD_AFFINITY_POLICY_COUNT = sizeof (thread_affinity_policy_data_t) div sizeof (integer_t);

// task_info.h

{
 *	Generic information structure to allow for expansion.
 *}

type
  task_flavor_t  = natural_t;
  task_info_t    = ^integer_t;		// varying array of int

const
  TASK_INFO_MAX	   = 1024;		// maximum array size
type
  task_info_data_t = array [0..TASK_INFO_MAX-1] of Integer;

{Currently defined information structures.}

//#pragma pack(4)
const
  TASK_BASIC_INFO_32     = 4;      // basic information
const
  TASK_BASIC2_INFO_32    = 6;

type
  _task_basic_info_32 = record
    suspend_count  : integer_t;    // suspend count for task
    virtual_size   : natural_t;    // virtual memory size (bytes)
    resident_size  : natural_t;    // resident memory size (bytes)
    user_time      : time_value_t; // total user run time for terminated threads
    system_time    : time_value_t; // total system run time for terminated threads
	  policy         : policy_t;		 // default policy for new threads */
  end;
  task_basic_info_32_data_t = _task_basic_info_32;
  task_basic_info_32_t = ^_task_basic_info_32;

const
  TASK_BASIC_INFO_32_COUNT  = sizeoF(task_basic_info_32_data_t) div sizeof(natural_t);


const
  TASK_BASIC_INFO_64      = 5;      // 64-bit capable basic info
type
  _task_basic_info_64 = record
    suspend_count  : integer_t;       // suspend count for task
    virtual_size   : mach_vm_size_t;  // virtual memory size (bytes)
    resident_size  : mach_vm_size_t;  // resident memory size (bytes)
    user_time      : time_value_t;    // total user run time for terminated threads
    system_time    : time_value_t;    // total system run time for terminated threads
   	policy         : policy_t;		    // default policy for new threads
  end;
  task_basic_info_64_data_t = _task_basic_info_64;
  task_basic_info_64_t = ^_task_basic_info_64;
const
  TASK_BASIC_INFO_64_COUNT = sizeof(task_basic_info_64_data_t) div sizeof(natural_t);


// localized structure - cannot be safely passed between tasks of differing sizes

type
  task_basic_info = record
    suspend_count : integer_t;    { suspend count for task }
    virtual_size  : vm_size_t;    { virtual memory size (bytes) }
    resident_size : vm_size_t;    { resident memory size (bytes) }
    user_time     : time_value_t; { total user run time for terminated threads }
    system_time   : time_value_t; { total system run time for terminated threads }
	  policy        : policy_t; 		{ default policy for new threads }
  end;
  task_basic_info_data_t = task_basic_info;
  task_basic_info_t = ^task_basic_info;
const
  TASK_BASIC_INFO_COUNT =(sizeof(task_basic_info_data_t) div sizeof(natural_t));

{#if !defined(__LP64__)
#define TASK_BASIC_INFO TASK_BASIC_INFO_32
#else
#define TASK_BASIC_INFO TASK_BASIC_INFO_64
#endif}

const
	TASK_EVENTS_INFO	= 2;	{ various event counts }
type
  _task_events_info = record
  	faults            : integer_t;	{ number of page faults }
	  pageins           : integer_t;	{ number of actual pageins }
	  cow_faults        : integer_t;	{ number of copy-on-write faults }
	  messages_sent     : integer_t;	{ number of messages sent }
	  messages_received : integer_t;  { number of messages received }
    syscalls_mach     : integer_t;  { number of mach system calls }
	  syscalls_unix     : integer_t;  { number of unix system calls }
	  csw               : integer_t;  { number of context switches }
  end;
  task_events_info_data_t = _task_events_info;
  task_events_info_t  = ^_task_events_info;
const
  TASK_EVENTS_INFO_COUNT =	(sizeof(task_events_info_data_t) div sizeof(natural_t));

const
	TASK_THREAD_TIMES_INFO	= 3;	{ total times for live threads - only accurate if suspended }

type
  _task_thread_times_info = record
  	user_time   : time_value_t;	{ total user run time for live threads }
	  system_time : time_value_t;	{ total system run time for live threads }
  end;

  task_thread_times_info_data_t = _task_thread_times_info;
  task_thread_times_info_t = ^_task_thread_times_info;
const
  TASK_THREAD_TIMES_INFO_COUNT=(sizeof(task_thread_times_info_data_t) div sizeof(natural_t));


const
  TASK_ABSOLUTETIME_INFO	= 1;
type
  _task_absolutetime_info = record
  	total_user      : uint64_t;		{ total time }
  	total_system    : uint64_t;
  	threads_user    : uint64_t;{ existing threads only }
  	threads_system  : uint64_t;
  end;

  task_absolutetime_info_data_t = _task_absolutetime_info;
  task_absolutetime_info_t = ^_task_absolutetime_info;
const
  TASK_ABSOLUTETIME_INFO_COUNT = (sizeof (task_absolutetime_info_data_t) div sizeof (natural_t));


{const
  TASK_SECURITY_TOKEN	      =	13;
  TASK_SECURITY_TOKEN_COUNT	= (sizeof(security_token_t) div sizeof(natural_t));}


{const
  TASK_AUDIT_TOKEN		    = 15;
  TASK_AUDIT_TOKEN_COUNT	= (sizeof(audit_token_t) / sizeof(natural_t));}


const
  TASK_AFFINITY_TAG_INFO	= 16;	{ This is experimental. }

type
  _task_affinity_tag_info = record
  	set_count   : integer_t;
  	min         : integer_t;
  	max         : integer_t;
  	task_count  : integer_t;
  end;
  task_affinity_tag_info_data_t = _task_affinity_tag_info;
  task_affinity_tag_info_t = ^_task_affinity_tag_info;
const
  TASK_AFFINITY_TAG_INFO_COUNT =	(sizeof(task_affinity_tag_info_data_t) div sizeof(natural_t));

//#pragma pack()

{ Obsolete interfaces.}
{
const
  TASK_SCHED_TIMESHARE_INFO	= 10;
  TASK_SCHED_RR_INFO		    = 11;
  TASK_SCHED_FIFO_INFO		  = 12;
  TASK_SCHED_INFO			      = 14;
}

// thread_state.h

const
  PPC_THREAD_STATE_MAX  = 144;    { Size of biggest state possible }
  I386_THREAD_STATE_MAX	= 144;    { Size of biggest state possible }
  // ARM ?

  // todo: should be platform dependant {$ifdef}
  THREAD_STATE_MAX = I386_THREAD_STATE_MAX;

// thread_status.h

type
  thread_state_t = ^natural_t;	{ Variable-length array }

  { THREAD_STATE_MAX is now defined in <mach/machine/thread_state.h> }
  thread_state_data_t = array [0..THREAD_STATE_MAX-1] of 	natural_t;

const
 	THREAD_STATE_FLAVOR_LIST	   = 0;	 { List of valid flavors }
  THREAD_STATE_FLAVOR_LIST_NEW = 128;

type
  thread_state_flavor_t =	integer;

  Tthread_state_flavor_array = array [word] of thread_state_flavor_t;
  Pthread_state_flavor_array = ^Tthread_state_flavor_array;
  //thread_state_flavor_array_t = ^thread_state_flavor_t;
  thread_state_flavor_array_t = Pthread_state_flavor_array;

{ **
  ** Process Management Interface
  ** }

// on-line manual pages:
// http://web.mit.edu/darwin/src/modules/xnu/osfmk/man/

{ *
  * Task Interface
  * }

type
  exception_type_t = integer;
  exception_data_type_t = integer_t;
  mach_exception_data_type_t = int64_t;
  exception_behavior_t = integer;
  exception_data_t = ^exception_data_type_t;
  mach_exception_data_t = ^mach_exception_data_type_t;
  exception_mask_t = LongWord;

  Texception_mask_array = array [word] of exception_mask_t;
  Pexception_mask_array = ^Texception_mask_array;
  exception_mask_array_t = Pexception_mask_array;
  //exception_mask_array_t = ^exception_mask_t; // original declaration

  Texception_behavior_array = array [word] of exception_behavior_t;
  Pexception_behavior_array = ^Texception_behavior_array;
  exception_behavior_array_t = Pexception_behavior_array;
  //exception_behavior_array_t = ^exception_behavior_t; // original declaration

  Texception_flavor_array = array [word] of thread_state_flavor_t;
  Pexception_flavor_array = ^Texception_flavor_array;
  exception_flavor_array_t = Pexception_flavor_array;
  //exception_flavor_array_t = ^thread_state_flavor_t; // original declaration

  Texception_port_array = array [word] of mach_port_t;
  Pexception_port_array = ^Texception_port_array;
  //exception_port_array_t = ^mach_port_t;
  exception_port_array_t = Pexception_port_array;

  mach_exception_code_t = ^mach_exception_data_type_t;
  mach_exception_subcode_t = ^mach_exception_data_type_t;


//mach_ports_lookup - Provide caller with an array of the target task's well-known ports.
function mach_ports_lookup(target_task: task_t; var init_port_set: mach_port_array_t;
	var init_port_setCnt : mach_msg_type_number_t): kern_return_t;
  cdecl external name 'mach_ports_lookup';

//mach_ports_register - Register an array of well-known ports on behalf of the target task.
function mach_ports_register(target_task: task_t;
  init_port_set: mach_port_array_t;	init_port_setCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'mach_ports_register';

//mach_task_self - Return a send right to the caller's task_self port.
function mach_task_self: mach_port_t; cdecl external name 'mach_task_self';

//task_create - Create a new task.
function task_create(target_task: task_t;
  ledgers : ledger_array_t; ledgersCnt : mach_msg_type_number_t;
  inherit_memory : boolean_t; var child_task : task_t): kern_return_t; cdecl external name 'task_create';

//task_get_emulation_vector - Return an array identifying the target task's user-level system call handlers.
function task_get_emulation_vector (task: task_t; vector_start: Pinteger;
  var emulation_vector: emulation_vector_t; var emulation_vectorCnt: mach_msg_type_number_t
): kern_return_t; cdecl external name 'task_get_emulation_vector';

//task_get_exception_ports - Return send rights to the target task's exception ports.
function task_get_exception_ports (task: task_t; exception_mask : exception_mask_t;
	masks : exception_mask_array_t;	var masksCnt: mach_msg_type_number_t;
	old_handlers: exception_handler_array_t; old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'task_get_exception_ports';

//task_get_special_port - Return a send write to the indicated special port.
function task_get_special_port(task: task_t;which_port: integer;
  var special_port: mach_port_t): kern_return_t; cdecl external name 'task_get_special_port';

//task_info - Return per-task information according to specified flavor.
function task_info(task: task_t; flavor: task_flavor_t; task_info: task_info_t;
  var task_info_count: mach_msg_type_number_t): kern_return_t; cdecl external name 'task_info';

//task_resume - Decrement the target task's suspend count.

function task_resume(target_task: task_t): kern_return_t;
  cdecl external name 'task_resume';


//task_sample - Sample the target task's thread program counters periodically.
function task_sample(task: task_t;reply: mach_port_t): kern_return_t;
  cdecl external name 'task_sample';

//task_set_emulation - Establish a user-level handler for a system call.
function task_set_emulation(target_port: task_t; routine_entry_pt: vm_address_t;
  routine_number: integer): kern_return_t; cdecl external name 'task_set_emulation';


//task_set_emulation_vector - Establish the target task's user-level system call handlers.
function task_set_emulation_vector (task: task_t; vector_start: integer;
  emulation_vector: emulation_vector_t; emulation_vectorCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_set_emulation_vector';

//task_set_exception_ports - Set target task's exception ports.
function task_set_exception_ports(task: task_t; 	exception_mask: exception_mask_t;
	new_port: mach_port_t; behavior: exception_behavior_t;
  new_flavor: thread_state_flavor_t): kern_return_t;
  cdecl external name 'task_set_exception_ports';


//task_set_info - Set task-specific information state.
function task_set_info(target_task: task_t; flavor: task_flavor_t;
  task_info_in: task_info_t; task_info_inCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_set_info';


//task_set_port_space - Set the size of the target task's port name space table.
// where?

//task_set_special_port - Set the indicated special port.
function task_set_special_port(task : task_t; which_port: integer;
  special_port: mach_port_t): kern_return_t; cdecl external name 'task_set_special_port';

//task_suspend - Suspend the target task.
function task_suspend(target_task: task_t): kern_return_t; cdecl
  external name 'task_suspend';

//task_swap_exception_ports - Set target task's exception ports, returning the previous exception ports.
function task_swap_exception_ports(task: task_t; exception_mask: exception_mask_t;
	new_port: mach_port_t; 	behavior: exception_behavior_t; new_flavor: thread_state_flavor_t;
	masks: exception_mask_array_t; var masksCnt:  mach_msg_type_number_t;
	old_handlerss: exception_handler_array_t;	old_behaviors: exception_behavior_array_t;
	old_flavors:   exception_flavor_array_t): kern_return_t;
  cdecl external name 'task_swap_exception_ports';

//task_terminate - Terminate the target task and deallocate its resources.
function task_terminate(target_task: task_t): kern_return_t;
  cdecl external name 'task_terminate';

//task_threads - Return the target task's list of threads.
function task_threads(target_task: task_t; var act_list: thread_act_array_t;
  var act_listCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'task_threads';

{ *
  * Thread Interface
  * }


// thread_info.h
{*
 *	Generic information structure to allow for expansion.
 *}
type
  thread_flavor_t =	natural_t	;
  thread_info_t = ^integer_t;		{ varying array of int }

const
  THREAD_INFO_MAX	=	1024; 	{ maximum array size *}
type
  thread_info_data_t = array [0..THREAD_INFO_MAX-1] of integer_t;

{*
 *	Currently defined information.
 *}
const
  THREAD_BASIC_INFO         =	3;   { basic information }

type
// struct
  _thread_basic_info = packed record
    user_time   : time_value_t; { user run time }
    system_time : time_value_t; { system run time }
    cpu_usage   : integer_t;    { scaled cpu usage percentage }
	  policy      : policy_t;		  { scheduling policy in effect }
    run_state   : integer_t;    { run state (see below) }
    flags       : integer_t;    { various flags (see below) }
    suspend_count : integer_t;  { suspend count for thread }
    sleep_time    : integer_t;  { number of seconds that thread has been sleeping }
  end;

  thread_basic_info_data_t =  _thread_basic_info;
  thread_basic_info_t = ^_thread_basic_info;

const
  THREAD_BASIC_INFO_COUNT = sizeof(thread_basic_info_data_t) div sizeof(natural_t);

{*
 *	Scale factor for usage field.
 *}

const
  TH_USAGE_SCALE =	1000;

{*
 *	Thread run states (state field).
 *}

const
  TH_STATE_RUNNING	= 1;	{ thread is running normally }
  TH_STATE_STOPPED	= 2;	{ thread is stopped }
  TH_STATE_WAITING	= 3;	{ thread is waiting normally }
  TH_STATE_UNINTERRUPTIBLE = 4;	{ thread is in an uninterruptible wait }
  TH_STATE_HALTED		= 5;	{ thread is halted at a clean point {

{*
 *	Thread flags (flags field).
 *}
  TH_FLAGS_SWAPPED = $01;	{ thread is swapped out }
  TH_FLAGS_IDLE		 = $02;	{ thread is an idle thread }

{*
 * Obsolete interfaces.
 *}

  THREAD_SCHED_TIMESHARE_INFO	= 10;
  THREAD_SCHED_RR_INFO		    = 11;
  THREAD_SCHED_FIFO_INFO		  = 12;


// thread_act.h

const
  // platform dependant?
 	thread_act_MSG_COUNT =	25;

// thread_terminate - Destroy a thread.
function thread_terminate(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_terminate';

function act_get_state (target_act: thread_act_t;flavor: integer;
	old_state: thread_state_t; var old_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'act_get_state';

function act_set_state (target_act: thread_act_t;	flavor: integer;
	new_state: thread_state_t; new_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'act_set_state';

// thread_get_state - Return the execution state for a thread.
function thread_get_state(target_act: thread_act_t; flavor: thread_state_flavor_t;
	old_state: thread_state_t; var old_stateCnt: mach_msg_type_number_t
  ): kern_return_t;
  cdecl external name 'thread_get_state';

//thread_set_state - Set the target thread's user-mode execution state.
function thread_set_state(target_act: thread_act_t;
	flavor: thread_state_flavor_t; new_state: thread_state_t;
	new_stateCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_set_state';

//thread_suspend - Suspend a thread.
function thread_suspend(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_suspend';

//thread_resume - Resume a thread.
function thread_resume(target_act: thread_act_t): kern_return_t ;
  cdecl external name 'thread_resume';

//thread_abort - Abort a thread.
function thread_abort(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_abort';

//thread_abort_safely - Abort a thread, restartably
function thread_abort_safely(target_act: thread_act_t): kern_return_t;
  cdecl external name 'thread_abort_safely';

// thread_depress_abort - Cancel thread scheduling depression.
function thread_depress_abort(thread: thread_act_t): kern_return_t;
  cdecl external name 'thread_depress_abort';

//thread_get_special_port - Return a send right to the caller-specified special port.
function thread_get_special_port (thr_act: thread_act_t; which_port: integer;
  var special_port: mach_port_t): kern_return_t ;
  cdecl external name 'thread_get_special_port';

//thread_set_special_port - Set caller-specified special port belonging to the target thread.
function thread_set_special_port(thr_act: thread_act_t;	which_port: integer;
	special_port: mach_port_t): kern_return_t;
  cdecl external name 'thread_set_special_port';

//thread_info - Return information about a thread.
function thread_info(target_act: thread_act_t;flavor: thread_flavor_t;
  thread_info_out: thread_info_t;
  var thread_info_outCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_info';

//thread_set_exception_ports - Set exception ports for a thread.
function thread_set_exception_ports(thread: thread_act_t;
  exception_mask: exception_mask_t; new_port:	mach_port_t;
	behavior: exception_behavior_t; new_flavor:	thread_state_flavor_t): kern_return_t;
  cdecl external name 'thread_set_exception_ports';

//thread_get_exception_ports - Return a send right to an exception port.
function thread_get_exception_ports(thread: thread_act_t;
	exception_mask: exception_mask_t;  masks: exception_mask_array_t;
	var masksCnt: mach_msg_type_number_t;	old_handlers: exception_handler_array_t;
	old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'thread_get_exception_ports';

//thread_swap_exception_ports - Swap exception ports for a thread.
function thread_swap_exception_ports(thread: thread_act_t;
	exception_mask: exception_mask_t; new_port: mach_port_t;
	behavior: exception_behavior_t; new_flavor: thread_state_flavor_t;
	masks: exception_mask_array_t; 	var masksCnt: mach_msg_type_number_t;
	old_handlers: exception_handler_array_t; old_behaviors: exception_behavior_array_t;
  old_flavors: exception_flavor_array_t): kern_return_t;
  cdecl external name 'thread_swap_exception_ports';

// Scheduling Thread Interface

// thread_policy - Set target thread's scheduling policy state.
function thread_policy (thr_act: thread_act_t; policy: policy_t;
	base: policy_base_t; baseCnt: mach_msg_type_number_t;
	set_limit: boolean_t): kern_return_t;
  cdecl external name 'thread_policy';

// ??? where?, don't mix with thread_set_policy()
function thread_policy_set(thread: thread_act_t;
	flavor: thread_policy_flavor_t; policy_info: thread_policy_t;
	policy_infoCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_policy_set';

//??? where?
function thread_policy_get(thread: thread_act_t; flavor: thread_policy_flavor_t;
	policy_info: thread_policy_t;	var policy_infoCnt: mach_msg_type_number_t;
	var get_default : boolean_t): kern_return_t;
  cdecl external name 'thread_policy_get';

//thread_sample - Perform periodic PC sampling for a thread.
function thread_sample(thread: thread_act_t;
  reply: mach_port_t): kern_return_t;
  cdecl external name 'thread_sample';

function etap_trace_thread(target_act: thread_act_t;
  trace_status: boolean_t): kern_return_t;
  cdecl external name 'etap_trace_thread';

// thread_assign - Assign a thread to a processor set.
function thread_assign(thread: thread_act_t;
  new_set: processor_set_t): kern_return_t;
  cdecl external name 'thread_assign';

//thread_assign_default - Assign a thread to the default processor set.
function thread_assign_default(thread: thread_act_t): kern_return_t;
  cdecl external name 'thread_assign_default';

// thread_get_assignment - Return the processor set to which a thread is assigned.
function thread_get_assignment(thread: thread_act_t;
  var assigned_set: processor_set_name_t): kern_return_t;
  cdecl external name 'thread_get_assignment';

// thread_set_policy - Set target thread's scheduling policy state.
function thread_set_policy(thr_act: thread_act_t;	pset: processor_set_t;
	policy: policy_t; base: policy_base_t; 	baseCnt: mach_msg_type_number_t;
	limit: policy_limit_t; limitCnt: mach_msg_type_number_t): kern_return_t;
  cdecl external name 'thread_set_policy';


// mach_traps.h

function swtch_pri(pri: integer): boolean_t; cdecl external name 'swtch_pri';

function switch: boolean_t; cdecl external name 'switch';

function thread_switch(thread_name: mach_port_name_t;	option: integer;
	option_time: mach_msg_timeout_t): kern_return_t; cdecl external name 'thread_switch';

function task_self_trap: mach_port_name_t; cdecl external name 'task_self_trap';

{*
 *	Obsolete interfaces.
 *}

function task_for_pid(target_tport: mach_port_name_t; pid: integer;
  var t: mach_port_name_t): kern_return_t; cdecl external name 'task_for_pid';

function task_name_for_pid(target_tport: mach_port_name_t; pid: integer;
	var tn: mach_port_name_t): kern_return_t; cdecl external name 'task_name_for_pid';

function pid_for_task(t: mach_port_name_t; var x: integer): kern_return_t;
  cdecl external name 'pid_for_task';

//#if		!defined(__LP64__)
// these should go away altogether - so no 64 legacy please */

function map_fd(fd: integer; offset: vm_offset_t;	var va: vm_offset_t;
	findspace: boolean_t; 	size: vm_size_t): kern_return_t;
  cdecl external name 'map_fd';

//#endif	/* !defined(__LP64__) */



implementation

end.

