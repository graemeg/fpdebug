unit mach_port;

{$mode objfpc}{$H+}

//todo: unite with machapi
//todo: mach_port_space_info (search for it below)

interface

uses
  machapi;

const
  mach_port_MSG_COUNT	= 28;

{* Routine mach_port_names *}
function mach_port_names(task: ipc_space_t; names: pmach_port_name_array_t;
  namesCnt: pmach_msg_type_number_t;  types: pmach_port_type_array_t;
  typesCnt: pmach_msg_type_number_t): kern_return_t; cdecl; external;

{* Routine mach_port_type *}
function mach_port_type (task: ipc_space_t;name: mach_port_name_t;
  ptype: pmach_port_type_t): kern_return_t; cdecl; external;

{* Routine mach_port_rename *}
function mach_port_rename (task: ipc_space_t;	old_name: mach_port_name_t;
  new_name: mach_port_name_t): kern_return_t; cdecl; external;

{* Routine mach_port_allocate_name *}
function mach_port_allocate_name
(
	task: ipc_space_t;
	right: mach_port_right_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_allocate *}
function mach_port_allocate
(
	task: ipc_space_t;
	right: mach_port_right_t;
	var name: mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_destroy *}
function mach_port_destroy(
	task: ipc_space_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_deallocate *}
function mach_port_deallocate
(
	task: ipc_space_t;
	name: mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_refs *}
function mach_port_get_refs
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	right : mach_port_right_t;
	refs  : pmach_port_urefs_t
): kern_return_t; cdecl; external;

{* Routine mach_port_mod_refs *}
function mach_port_mod_refs
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	right : mach_port_right_t;
	delta : mach_port_delta_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_mscount *}
function mach_port_set_mscount
(
	task    : ipc_space_t;
	name    : mach_port_name_t;
	mscount : mach_port_mscount_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_set_status *}
function mach_port_get_set_status
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	members    : pmach_port_name_array_t;
	membersCnt : pmach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_move_member *}
function mach_port_move_member
(
	task    : ipc_space_t;
	member  : mach_port_name_t;
	after   :mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_request_notification *}
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

{* Routine mach_port_insert_right *}
function mach_port_insert_right
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	poly  : mach_port_t;
	polyPoly  : mach_msg_type_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_extract_right *}
function mach_port_extract_right
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	msgt_name : mach_msg_type_name_t;
	poly      : pmach_port_t;
	polyPoly  : pmach_msg_type_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_seqno *}
function mach_port_set_seqno
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	seqno : mach_port_seqno_t
): kern_return_t; cdecl; external;

{* Routine mach_port_get_attributes *}
function mach_port_get_attributes
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	flavor  : mach_port_flavor_t;
	port_info_out : mach_port_info_t;
	port_info_outCnt  : pmach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_set_attributes *}
function mach_port_set_attributes
(
	task      : ipc_space_t;
	name      : mach_port_name_t;
	flavor    : mach_port_flavor_t;
	port_info : mach_port_info_t;
	port_infoCnt  : mach_msg_type_number_t
): kern_return_t; cdecl; external;

{* Routine mach_port_allocate_qos *}
function mach_port_allocate_qos
(
	task: ipc_space_t;
	right: mach_port_right_t;
	qos: pmach_port_qos_t;
	name: pmach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_allocate_full *}
function mach_port_allocate_full
(
	task  : ipc_space_t;
	right : mach_port_right_t;
	proto : mach_port_t;
	qos   : pmach_port_qos_t;
	name  : pmach_port_name_t
): kern_return_t; cdecl; external;

{* Routine task_set_port_space *}
function task_set_port_space
(
	task : ipc_space_t;
	table_entries : integer
): kern_return_t; cdecl; external;

{* Routine mach_port_get_srights *}
function mach_port_get_srights
(
	task    : ipc_space_t;
	name    : mach_port_name_t;
	srights : pmach_port_rights_t
): kern_return_t; cdecl; external;

{* Routine mach_port_space_info *}
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

{* Routine mach_port_dnrequest_info *}
function mach_port_dnrequest_info
(
	task : ipc_space_t;
	name : mach_port_name_t;
	dnr_total : plongword;
	dnr_used  : plongword
): kern_return_t; cdecl; external;

{* Routine mach_port_kernel_object *}
function mach_port_kernel_object
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	object_type : PLongWord;
	object_addr : pvm_offset_t
): kern_return_t; cdecl; external;

{* Routine mach_port_insert_member *}
function mach_port_insert_member
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	pset  : mach_port_name_t
): kern_return_t; cdecl; external;

{* Routine mach_port_extract_member *}
function mach_port_extract_member
(
	task  : ipc_space_t;
	name  : mach_port_name_t;
	pset  : mach_port_name_t
): kern_return_t; cdecl; external;

implementation

end.

