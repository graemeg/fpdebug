unit mach_port;

{$mode objfpc}{$H+}

//todo: unite with machapi
//todo: mach_port_space_info (search for it below)

interface

uses
  machapi;

const
  mach_port_MSG_COUNT	= 28;

{* mach_port_names
 *	Returns the set of port and port set names
 *	to which the target task has access, along with
 *	the type (set or port) for each name.
 *}
function mach_port_names(task: ipc_space_t; names: pmach_port_name_array_t;
  namesCnt: pmach_msg_type_number_t;  types: pmach_port_type_array_t;
  typesCnt: pmach_msg_type_number_t): kern_return_t; cdecl; external;

{* mach_port_type
 *	Returns the type (set or port) for the port name
 *	within the target task.  Also indicates whether
 *	there is a dead-name request for the name.
*}
function mach_port_type (task: ipc_space_t;name: mach_port_name_t;
  ptype: pmach_port_type_t): kern_return_t; cdecl; external;

{* mach_port_rename
 *	Changes the name by which a port (or port set) is known to
 *	the target task.  The new name can't be in use.  The
 *	old name becomes available for recycling.
 *}
function mach_port_rename (task: ipc_space_t;	old_name: mach_port_name_t;
  new_name: mach_port_name_t): kern_return_t; cdecl; external;

{* mach_port_allocate_name
 *	Allocates the specified kind of object, with the given name.
 *	The right must be one of
 *		MACH_PORT_RIGHT_RECEIVE
 *		MACH_PORT_RIGHT_PORT_SET
 *		MACH_PORT_RIGHT_DEAD_NAME
 *	New port sets are empty.  New ports don't have any
 *	send/send-once rights or queued messages.  The make-send
 *	count is zero and their queue limit is MACH_PORT_QLIMIT_DEFAULT.
 *	New sets, ports, and dead names have one user reference.
*}
function mach_port_allocate_name
(
	task: ipc_space_t;
	right: mach_port_right_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* mach_port_allocate
 *	Allocates the specified kind of object.
 *	The right must be one of
 *		MACH_PORT_RIGHT_RECEIVE
 *		MACH_PORT_RIGHT_PORT_SET
 *		MACH_PORT_RIGHT_DEAD_NAME
 *	Like port_allocate_name, but the kernel picks a name.
 *	It can use any name not associated with a right.
*}
function mach_port_allocate
(
	task: ipc_space_t;
	right: mach_port_right_t;
	var name: mach_port_name_t
): kern_return_t; cdecl; external;

{* mach_port_destroy
 *	Destroys all rights associated with the name and makes it
 *	available for recycling immediately.  The name can be a
 *	port (possibly with multiple user refs), a port set, or
 *	a dead name (again, with multiple user refs).
*}
function mach_port_destroy(
	task: ipc_space_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* mach_port_deallocate
 *	Releases one send/send-once/dead-name user ref.
 *	Just like mach_port_mod_refs -1, but deduces the
 *	correct type of right.  This allows a user task
 *	to release a ref for a port without worrying
 *	about whether the port has died or not.
*}
function mach_port_deallocate
(
	task: ipc_space_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_refs
 *	A port set always has one user ref.
 *	A send-once right always has one user ref.
 *	A dead name always has one or more user refs.
 *	A send right always has one or more user refs.
 *	A receive right always has one user ref.
 *	The right must be one of
 *		MACH_PORT_RIGHT_RECEIVE
 *		MACH_PORT_RIGHT_PORT_SET
 *		MACH_PORT_RIGHT_DEAD_NAME
 *		MACH_PORT_RIGHT_SEND
 *		MACH_PORT_RIGHT_SEND_ONCE
*}
function mach_port_get_refs
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	right : mach_port_right_t;
	refs  : pmach_port_urefs_t
): kern_return_t; cdecl; external;

{* Routine mach_port_mod_refs
 *	The delta is a signed change to the task's
 *	user ref count for the right.  Only dead names
 *	and send rights can have a positive delta.
 *	The resulting user ref count can't be negative.
 *	If it is zero, the right is deallocated.
 *	If the name isn't a composite right, it becomes
 *	available for recycling.  The right must be one of
 *		MACH_PORT_RIGHT_RECEIVE
 *		MACH_PORT_RIGHT_PORT_SET
 *		MACH_PORT_RIGHT_DEAD_NAME
 *		MACH_PORT_RIGHT_SEND
 *		MACH_PORT_RIGHT_SEND_ONCE
*}
function mach_port_mod_refs
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	right : mach_port_right_t;
	delta : mach_port_delta_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_mscount
 *	Only valid for receive rights.
 *	Sets the make-send count for the port.
*}
function mach_port_set_mscount
(
	task    : ipc_space_t;
	name    : mach_port_name_t;
	mscount : mach_port_mscount_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_set_status
 *	Only valid for port sets.  Returns a list of
 *	the members.
*}
function mach_port_get_set_status
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	members    : pmach_port_name_array_t;
	membersCnt : pmach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_move_member
 *	Puts the member port (the task must have receive rights)
 *	into the after port set.  If the port is already a member
 *	of any set(s), it is atomically removed from those sets as
 *	part of this operation.  (If after is MACH_PORT_NULL, the
 *	port is still removed from all current sets).
*}
function mach_port_move_member
(
	task    : ipc_space_t;
	member  : mach_port_name_t;
	after   :mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_request_notification
 *	Requests a notification from the kernel.  The request
 *	must supply the send-once right which is used for
 *	the notification.  If a send-once right was previously
 *	registered, it is returned.  The msg_id must be one of
 *		MACH_NOTIFY_PORT_DESTROYED (receive rights)
 *		MACH_NOTIFY_DEAD_NAME (send/receive/send-once rights)
 *		MACH_NOTIFY_NO_SENDERS (receive rights)
 *
 *	The sync value specifies whether a notification should
 *	get sent immediately, if appropriate.  The exact meaning
 *	depends on the notification:
 *		MACH_NOTIFY_PORT_DESTROYED: must be zero.
 *		MACH_NOTIFY_DEAD_NAME: if non-zero, then name can be dead,
 *			and the notification gets sent immediately.
 *			If zero, then name can't be dead.
 *		MACH_NOTIFY_NO_SENDERS: the notification gets sent
 *			immediately if the current mscount is greater
 *			than or equal to the sync value and there are no
 *			extant send rights.
*}
function mach_port_request_notification
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	msgid : mach_msg_id_t;
	sync  : mach_port_mscount_t;
	notify  : mach_port_t;
	notifyPoly  : mach_msg_type_name_t;
	previous  : pmach_port_t
): kern_return_t; cdecl; external;

{ Routine mach_port_insert_right  Inserts the specified rights into the target task,
 	using the specified name.  If inserting send/receive rights and the task already 
  has send/receive rights 	for the port, then the names must agree.  In any case,
 	the task gains a user ref for the port.                                         }
function mach_port_insert_right (
	task      : ipc_space_t;
	name      : mach_port_name_t;
	poly      : mach_port_t;
	polyPoly  : mach_msg_type_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_extract_right
 *	Returns the specified right for the named port
 *	in the target task, extracting that right from
 *	the target task.  The target task loses a user
 *	ref and the name may be available for recycling.
 *	msgt_name must be one of
 *		MACH_MSG_TYPE_MOVE_RECEIVE
 *		MACH_MSG_TYPE_COPY_SEND
 *		MACH_MSG_TYPE_MAKE_SEND
 *		MACH_MSG_TYPE_MOVE_SEND
 *		MACH_MSG_TYPE_MAKE_SEND_ONCE
 *		MACH_MSG_TYPE_MOVE_SEND_ONCE
*}
function mach_port_extract_right
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	msgt_name : mach_msg_type_name_t;
	poly      : pmach_port_t;
	polyPoly  : pmach_msg_type_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_seqno
 *	Only valid for receive rights.
 *	Sets the sequence number for the port.
*}
function mach_port_set_seqno
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	seqno : mach_port_seqno_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_attributes
 *      Returns information about a port.
*}
function mach_port_get_attributes
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	flavor  : mach_port_flavor_t;
	port_info_out : mach_port_info_t;
	port_info_outCnt  : pmach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_attributes
 *      Set attributes of a port
*}
function mach_port_set_attributes
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	flavor    : mach_port_flavor_t;
	port_info : mach_port_info_t;
	port_infoCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_allocate_qos
 *	Allocates the specified kind of object, qos version.
 *	The right must be
 *		MACH_PORT_RIGHT_RECEIVE
 *	Like port_allocate_name, but the kernel picks a name.
 *	It can use any name not associated with a right.
 *}
function mach_port_allocate_qos
(
	task: ipc_space_t;
	right: mach_port_right_t;
	qos: pmach_port_qos_t;
	name: pmach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_allocate_full
 *	Generic interface to allocation various kinds of ports.
 *	Should never be called directly by users (at least not
 *	unless they are exceedingly masochistic).
*}
function mach_port_allocate_full
(
	task  : ipc_space_t;
	right : mach_port_right_t;
	proto : mach_port_t;
	qos   : pmach_port_qos_t;
	name  : pmach_port_name_t
): kern_return_t; cdecl; external;

{* Routine task_set_port_space
 *	Pre-expand task port name space.
*}
function task_set_port_space
(
	task : ipc_space_t;
	table_entries : integer
): kern_return_t; cdecl; external;

{* Routine mach_port_get_srights
 *	Returns the exact number of extant send rights
 *	for the given receive right.
 *      This call is only valid on MACH_IPC_DEBUG kernels.
 *      Otherwise, KERN_FAILURE is returned.
*}
function mach_port_get_srights
(
	task    : ipc_space_t;
	name    : mach_port_name_t;
	srights : pmach_port_rights_t
): kern_return_t; cdecl; external;


{* Routine mach_port_space_info
 *	Returns information about an IPC space.
 *      This call is only valid on MACH_IPC_DEBUG kernels.
 *      Otherwise, KERN_FAILURE is returned.
*}

//TODO:!!!!

{function mach_port_space_info
(
	task : ipc_space_t;
	space_info    : pipc_info_space_t;
	table_info    : pipc_info_name_array_t;
	table_infoCnt : pmach_msg_type_number_t;
	tree_info     : pipc_info_tree_name_array_t;
	tree_infoCnt  : pmach_msg_type_number_t
): kern_return_t; cdecl; external;
}

{* Routine mach_port_dnrequest_info
 *	Returns information about the dead-name requests
 *	registered with the named receive right.
 *      This call is only valid on MACH_IPC_DEBUG kernels.
 *      Otherwise, KERN_FAILURE is returned.
*}
function mach_port_dnrequest_info
(
	task : ipc_space_t;
	name : mach_port_name_t;
	dnr_total : plongword;
	dnr_used  : plongword
): kern_return_t; cdecl; external;

{* Routine mach_port_kernel_object
 *	Return the type and address of the kernel object
 *	that the given send/receive right represents.
 *      This call is only valid on MACH_IPC_DEBUG kernels.
 *      Otherwise, KERN_FAILURE is returned.
*}
function mach_port_kernel_object
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	object_type : PLongWord;
	object_addr : pvm_offset_t
): kern_return_t; cdecl; external;

{* Routine mach_port_insert_member
 *	Inserts the specified rights into the portset identified
 *	by the <task, pset> pair.  The results of passing in the
 *	Poly argument via the supplied disposition must yield a
 *	receive right.
 *
 *	If the <task,pset> pair does not represent a valid portset
 *	KERN_INVALID_RIGHT is returned.
 *
 *	If the passed in name argument does not represent a receive
 *	right, KERN_INVALID_CAPABILITY will be returned.
 *
 *	If the port represented by the receive right is already in
 *	the portset, KERN_ALREADY_IN_SET is returned.
*}
function mach_port_insert_member
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	pset  : mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_extract_member
 *	Extracts the specified right from the named portset
 *	in the target task.
 *	the target task.  The target task loses a user
 *	ref and the name may be available for recycling.
 *	msgt_name must be one of
 *		MACH_MSG_TYPE_MOVE_RECEIVE
 *		MACH_MSG_TYPE_COPY_SEND
 *		MACH_MSG_TYPE_MAKE_SEND
 *		MACH_MSG_TYPE_MOVE_SEND
 *		MACH_MSG_TYPE_MAKE_SEND_ONCE
 *		MACH_MSG_TYPE_MOVE_SEND_ONCE
*}
function mach_port_extract_member
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	pset  : mach_port_name_t
): kern_return_t; cdecl; external;

//todo:

implementation

end.

