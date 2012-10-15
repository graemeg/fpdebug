{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
unit mach;

{$mode objfpc}{$H+}

interface

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

{
  Automatically converted by H2Pas 1.0.0 from _structs.h
  The following command line parameters were used:
    _structs.h
}

  {
   * i386 is the structure that is exported to user threads for
   * use in status/mutate calls.  This structure should never change.
   *
    }
{$if __DARWIN_UNIX03}

type
  __darwin_i386_thread_state = record
    __eax, __ebx, __ecx, __edx : dword;
    __edi, __esi, __ebp, __esp : dword;
    __ss : dword;
    __eflags : dword;
    __eip : dword;
    __cs : dword;
    __ds : dword;
    __es : dword;
    __fs : dword;
    __gs : dword;
  end;
  _STRUCT_X86_THREAD_STATE32 = __darwin_i386_thread_state;

{$else}

type
  i386_thread_state = record
    eax : dword;
    ebx : dword;
    ecx : dword;
    edx : dword;
    edi : dword;
    esi : dword;
    ebp : dword;
    esp : dword;
    ss : dword;
    eflags : dword;
    eip : dword;
    cs : dword;
    ds : dword;
    es : dword;
    fs : dword;
    gs : dword;
  end;
  _STRUCT_X86_THREAD_STATE32 = i386_thread_state;

{$endif}

  { !__DARWIN_UNIX03  }
  { This structure should be double-word aligned for performance  }

const
  FP_PREC_24B = 0;
  FP_PREC_53B = 2;
  FP_PREC_64B = 3;

  FP_RND_NEAR = 0;
  FP_RND_DOWN = 1;
  FP_RND_UP = 2;
  FP_CHOP = 3;

  __darwin_fp_status = record
    flag0 : word;
  end;

  _STRUCT_FP_STATUS = __darwin_fp_status;
   __darwin_fp_status_t = _STRUCT_FP_STATUS;

type
  fp_status = record
    flag0 : word;
  end;
  fp_status_t = fp_status;
  _STRUCT_FP_STATUS = fp_status_t;

{$endif}

{ defn of 80bit x87 FPU or MMX register   }

{$if __DARWIN_UNIX03}

type
  __darwin_mmst_reg = record
    __mmst_reg : array[0..9] of char;
    __mmst_rsrv : array[0..5] of char;
  end;
  _STRUCT_MMST_REG = __darwin_mmst_reg;

{$else}

type
  mmst_reg = record
    mmst_reg : array[0..9] of char;
    mmst_rsrv : array[0..5] of char;
  end;
  _STRUCT_MMST_REG = mmst_reg;

{$endif}

{$if __DARWIN_UNIX03}
type
  __darwin_xmm_reg = record
    __xmm_reg : array[0..15] of char;
  end;

{$else}
type
  xmm_reg = record
    xmm_reg : array[0..15] of char;
  end;
  _STRUCT_XMM_REG = xmm_reg;

{$endif}
  {*  Floating point state.  }

{ number of chars worth of data from fpu_fcw  }
const
  FP_STATE_BYTES = 512;

{$if !defined(_POSIX_C_SOURCE) || defined(_DARWIN_C_SOURCE)}

{$endif}
  { !_POSIX_C_SOURCE || _DARWIN_C_SOURCE  }
{$if __DARWIN_UNIX03}

type
  __darwin_i386_float_state = record
    __fpu_reserved : array[0..1] of longint;
    __fpu_fcw : _STRUCT_FP_CONTROL;              { x87 FPU control word  }
    __fpu_fsw : _STRUCT_FP_STATUS;               { x87 FPU status word  }
    __fpu_ftw : __uint8_t;                       { x87 FPU tag word  }
    __fpu_rsrv1 : __uint8_t;                     { reserved  }
    __fpu_fop : __uint16_t;                      { x87 FPU Opcode  }
    __fpu_ip : __uint32_t;                       { x87 FPU Instruction Pointer offset  }
    __fpu_cs : __uint16_t;                       { x87 FPU Instruction Pointer Selector  }
    __fpu_rsrv2 : __uint16_t;                    { reserved  }
    __fpu_dp : __uint32_t;                       { x87 FPU Instruction Operand(Data) Pointer offset  }
    __fpu_ds : __uint16_t;                       { x87 FPU Instruction Operand(Data) Pointer Selector  }
    __fpu_rsrv3 : __uint16_t;                    { reserved  }
    __fpu_mxcsr : __uint32_t;                    { MXCSR Register state  }
    __fpu_mxcsrmask : __uint32_t;                { MXCSR mask  }
    __fpu_stmm0 : _STRUCT_MMST_REG;              { ST0/MM0    }
    __fpu_stmm1 : _STRUCT_MMST_REG;              { ST1/MM1   }
    __fpu_stmm2 : _STRUCT_MMST_REG;              { ST2/MM2   }
    __fpu_stmm3 : _STRUCT_MMST_REG;              { ST3/MM3   }
    __fpu_stmm4 : _STRUCT_MMST_REG;              { ST4/MM4   }
    __fpu_stmm5 : _STRUCT_MMST_REG;              { ST5/MM5   }
    __fpu_stmm6 : _STRUCT_MMST_REG;              { ST6/MM6   }
    __fpu_stmm7 : _STRUCT_MMST_REG;              { ST7/MM7   }
    __fpu_xmm0 : _STRUCT_XMM_REG;                { XMM 0   }
    __fpu_xmm1 : _STRUCT_XMM_REG;                { XMM 1   }
    __fpu_xmm2 : _STRUCT_XMM_REG;                { XMM 2   }
    __fpu_xmm3 : _STRUCT_XMM_REG;                { XMM 3   }
    __fpu_xmm4 : _STRUCT_XMM_REG;                { XMM 4   }
    __fpu_xmm5 : _STRUCT_XMM_REG;                { XMM 5   }
    __fpu_xmm6 : _STRUCT_XMM_REG;                { XMM 6   }
    __fpu_xmm7 : _STRUCT_XMM_REG;                { XMM 7   }
    __fpu_rsrv4 : array[0..(14*16)-1] of char;   { reserved  }
    __fpu_reserved1 : longint;
  end;
  _STRUCT_X86_FLOAT_STATE32 = __darwin_i386_float_state;

{$else}

type
 i386_float_state = record
    fpu_reserved : array[0..1] of longint;
    fpu_fcw : _STRUCT_FP_CONTROL;            { x87 FPU control word  }
    fpu_fsw : _STRUCT_FP_STATUS;             { x87 FPU status word  }
    fpu_ftw : __uint8_t;                     { x87 FPU tag word  }
    fpu_rsrv1 : __uint8_t;                   { reserved  }
    fpu_fop : __uint16_t;                    { x87 FPU Opcode  }
    fpu_ip : __uint32_t;                     { x87 FPU Instruction Pointer offset  }
    fpu_cs : __uint16_t;                     { x87 FPU Instruction Pointer Selector  }
    fpu_rsrv2 : __uint16_t;                  { reserved  }
    fpu_dp : __uint32_t;                     { x87 FPU Instruction Operand(Data) Pointer offset  }
    fpu_ds : __uint16_t;                     { x87 FPU Instruction Operand(Data) Pointer Selector  }
    fpu_rsrv3 : __uint16_t;                  { reserved  }
    fpu_mxcsr : __uint32_t;                  { MXCSR Register state  }
    fpu_mxcsrmask : __uint32_t;              { MXCSR mask  }
    fpu_stmm0 : _STRUCT_MMST_REG;            { ST0/MM0    }
    fpu_stmm1 : _STRUCT_MMST_REG;            { ST1/MM1   }
    fpu_stmm2 : _STRUCT_MMST_REG;            { ST2/MM2   }
    fpu_stmm3 : _STRUCT_MMST_REG;            { ST3/MM3   }
    fpu_stmm4 : _STRUCT_MMST_REG;            { ST4/MM4   }
    fpu_stmm5 : _STRUCT_MMST_REG;            { ST5/MM5   }
    fpu_stmm6 : _STRUCT_MMST_REG;            { ST6/MM6   }
    fpu_stmm7 : _STRUCT_MMST_REG;            { ST7/MM7   }
    fpu_xmm0 : _STRUCT_XMM_REG;              { XMM 0   }
    fpu_xmm1 : _STRUCT_XMM_REG;              { XMM 1   }
    fpu_xmm2 : _STRUCT_XMM_REG;              { XMM 2   }
    fpu_xmm3 : _STRUCT_XMM_REG;              { XMM 3   }
    fpu_xmm4 : _STRUCT_XMM_REG;              { XMM 4   }
    fpu_xmm5 : _STRUCT_XMM_REG;              { XMM 5   }
    fpu_xmm6 : _STRUCT_XMM_REG;              { XMM 6   }
    fpu_xmm7 : _STRUCT_XMM_REG;              { XMM 7   }
    fpu_rsrv4 : array[0..(14*16)-1] of char; { reserved  }
    fpu_reserved1 : longint;
  end;

{$endif}

{$if __DARWIN_UNIX03}

type
  __darwin_i386_exception_state = record
    __trapno : dword;
    __err : dword;
    __faultvaddr : dword;
  end;
  _STRUCT_X86_EXCEPTION_STATE32 = __darwin_i386_exception_state;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define  _STRUCT_X86_EXCEPTION_STATE32 }

  type
     i386_exception_state = record
          trapno : dword;
          err : dword;
          faultvaddr : dword;
       end;

{$endif}
  { !__DARWIN_UNIX03  }
{$if __DARWIN_UNIX03}
  {todo: #define  _STRUCT_X86_DEBUG_STATE32 }

  type
     __darwin_x86_debug_state32 = record
          __dr0 : dword;
          __dr1 : dword;
          __dr2 : dword;
          __dr3 : dword;
          __dr4 : dword;
          __dr5 : dword;
          __dr6 : dword;
          __dr7 : dword;
       end;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define  _STRUCT_X86_DEBUG_STATE32 }

  type
     x86_debug_state32 = record
          dr0 : dword;
          dr1 : dword;
          dr2 : dword;
          dr3 : dword;
          dr4 : dword;
          dr5 : dword;
          dr6 : dword;
          dr7 : dword;
       end;

{$endif}
  { !__DARWIN_UNIX03  }
  {
   * 64 bit versions of the above
    }
{$if __DARWIN_UNIX03}
  {todo: #define 	_STRUCT_X86_THREAD_STATE64 }

  type
     __darwin_x86_thread_state64 = record
          __rax : __uint64_t;
          __rbx : __uint64_t;
          __rcx : __uint64_t;
          __rdx : __uint64_t;
          __rdi : __uint64_t;
          __rsi : __uint64_t;
          __rbp : __uint64_t;
          __rsp : __uint64_t;
          __r8 : __uint64_t;
          __r9 : __uint64_t;
          __r10 : __uint64_t;
          __r11 : __uint64_t;
          __r12 : __uint64_t;
          __r13 : __uint64_t;
          __r14 : __uint64_t;
          __r15 : __uint64_t;
          __rip : __uint64_t;
          __rflags : __uint64_t;
          __cs : __uint64_t;
          __fs : __uint64_t;
          __gs : __uint64_t;
       end;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define 	_STRUCT_X86_THREAD_STATE64 }

  type
     x86_thread_state64 = record
          rax : __uint64_t;
          rbx : __uint64_t;
          rcx : __uint64_t;
          rdx : __uint64_t;
          rdi : __uint64_t;
          rsi : __uint64_t;
          rbp : __uint64_t;
          rsp : __uint64_t;
          r8 : __uint64_t;
          r9 : __uint64_t;
          r10 : __uint64_t;
          r11 : __uint64_t;
          r12 : __uint64_t;
          r13 : __uint64_t;
          r14 : __uint64_t;
          r15 : __uint64_t;
          rip : __uint64_t;
          rflags : __uint64_t;
          cs : __uint64_t;
          fs : __uint64_t;
          gs : __uint64_t;
       end;

{$endif}
  { !__DARWIN_UNIX03  }
{$if __DARWIN_UNIX03}
  {todo: #define	_STRUCT_X86_FLOAT_STATE64 }
  { x87 FPU control word  }
  { x87 FPU status word  }
  { x87 FPU tag word  }
  { reserved  }  { x87 FPU Opcode  }
  { x87 FPU Instruction Pointer  }
  { offset  }
  { Selector  }
  { reserved  }
  { x87 FPU Instruction Operand(Data) Pointer  }
  { offset  }
  { Selector  }
  { reserved  }
  { MXCSR Register state  }
  { MXCSR mask  }
  { ST0/MM0    }
  { ST1/MM1   }
  { ST2/MM2   }
  { ST3/MM3   }
  { ST4/MM4   }
  { ST5/MM5   }
  { ST6/MM6   }
  { ST7/MM7   }
  { XMM 0   }
  { XMM 1   }
  { XMM 2   }
  { XMM 3   }
  { XMM 4   }
  { XMM 5   }
  { XMM 6   }
  { XMM 7   }
  { XMM 8   }
  { XMM 9   }
  { XMM 10   }
  { XMM 11  }
  { XMM 12   }
  { XMM 13   }
  { XMM 14   }
  { XMM 15   }
  { reserved  }

  type
     __darwin_x86_float_state64 = record
          __fpu_reserved : array[0..1] of longint;
          __fpu_fcw : _STRUCT_FP_CONTROL;
          __fpu_fsw : _STRUCT_FP_STATUS;
          __fpu_ftw : __uint8_t;
          __fpu_rsrv1 : __uint8_t;
          __fpu_fop : __uint16_t;
          __fpu_ip : __uint32_t;
          __fpu_cs : __uint16_t;
          __fpu_rsrv2 : __uint16_t;
          __fpu_dp : __uint32_t;
          __fpu_ds : __uint16_t;
          __fpu_rsrv3 : __uint16_t;
          __fpu_mxcsr : __uint32_t;
          __fpu_mxcsrmask : __uint32_t;
          __fpu_stmm0 : _STRUCT_MMST_REG;
          __fpu_stmm1 : _STRUCT_MMST_REG;
          __fpu_stmm2 : _STRUCT_MMST_REG;
          __fpu_stmm3 : _STRUCT_MMST_REG;
          __fpu_stmm4 : _STRUCT_MMST_REG;
          __fpu_stmm5 : _STRUCT_MMST_REG;
          __fpu_stmm6 : _STRUCT_MMST_REG;
          __fpu_stmm7 : _STRUCT_MMST_REG;
          __fpu_xmm0 : _STRUCT_XMM_REG;
          __fpu_xmm1 : _STRUCT_XMM_REG;
          __fpu_xmm2 : _STRUCT_XMM_REG;
          __fpu_xmm3 : _STRUCT_XMM_REG;
          __fpu_xmm4 : _STRUCT_XMM_REG;
          __fpu_xmm5 : _STRUCT_XMM_REG;
          __fpu_xmm6 : _STRUCT_XMM_REG;
          __fpu_xmm7 : _STRUCT_XMM_REG;
          __fpu_xmm8 : _STRUCT_XMM_REG;
          __fpu_xmm9 : _STRUCT_XMM_REG;
          __fpu_xmm10 : _STRUCT_XMM_REG;
          __fpu_xmm11 : _STRUCT_XMM_REG;
          __fpu_xmm12 : _STRUCT_XMM_REG;
          __fpu_xmm13 : _STRUCT_XMM_REG;
          __fpu_xmm14 : _STRUCT_XMM_REG;
          __fpu_xmm15 : _STRUCT_XMM_REG;
          __fpu_rsrv4 : array[0..(6*16)-1] of char;
          __fpu_reserved1 : longint;
       end;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define	_STRUCT_X86_FLOAT_STATE64 }
  { x87 FPU control word  }
  { x87 FPU status word  }
  { x87 FPU tag word  }
  { reserved  }  { x87 FPU Opcode  }
  { x87 FPU Instruction Pointer  }
  { offset  }
  { Selector  }
  { reserved  }
  { x87 FPU Instruction Operand(Data) Pointer  }
  { offset  }
  { Selector  }
  { reserved  }
  { MXCSR Register state  }
  { MXCSR mask  }
  { ST0/MM0    }
  { ST1/MM1   }
  { ST2/MM2   }
  { ST3/MM3   }
  { ST4/MM4   }
  { ST5/MM5   }
  { ST6/MM6   }
  { ST7/MM7   }
  { XMM 0   }
  { XMM 1   }
  { XMM 2   }
  { XMM 3   }
  { XMM 4   }
  { XMM 5   }
  { XMM 6   }
  { XMM 7   }
  { XMM 8   }
  { XMM 9   }
  { XMM 10   }
  { XMM 11  }
  { XMM 12   }
  { XMM 13   }
  { XMM 14   }
  { XMM 15   }
  { reserved  }

  type
     x86_float_state64 = record
          fpu_reserved : array[0..1] of longint;
          fpu_fcw : _STRUCT_FP_CONTROL;
          fpu_fsw : _STRUCT_FP_STATUS;
          fpu_ftw : __uint8_t;
          fpu_rsrv1 : __uint8_t;
          fpu_fop : __uint16_t;
          fpu_ip : __uint32_t;
          fpu_cs : __uint16_t;
          fpu_rsrv2 : __uint16_t;
          fpu_dp : __uint32_t;
          fpu_ds : __uint16_t;
          fpu_rsrv3 : __uint16_t;
          fpu_mxcsr : __uint32_t;
          fpu_mxcsrmask : __uint32_t;
          fpu_stmm0 : _STRUCT_MMST_REG;
          fpu_stmm1 : _STRUCT_MMST_REG;
          fpu_stmm2 : _STRUCT_MMST_REG;
          fpu_stmm3 : _STRUCT_MMST_REG;
          fpu_stmm4 : _STRUCT_MMST_REG;
          fpu_stmm5 : _STRUCT_MMST_REG;
          fpu_stmm6 : _STRUCT_MMST_REG;
          fpu_stmm7 : _STRUCT_MMST_REG;
          fpu_xmm0 : _STRUCT_XMM_REG;
          fpu_xmm1 : _STRUCT_XMM_REG;
          fpu_xmm2 : _STRUCT_XMM_REG;
          fpu_xmm3 : _STRUCT_XMM_REG;
          fpu_xmm4 : _STRUCT_XMM_REG;
          fpu_xmm5 : _STRUCT_XMM_REG;
          fpu_xmm6 : _STRUCT_XMM_REG;
          fpu_xmm7 : _STRUCT_XMM_REG;
          fpu_xmm8 : _STRUCT_XMM_REG;
          fpu_xmm9 : _STRUCT_XMM_REG;
          fpu_xmm10 : _STRUCT_XMM_REG;
          fpu_xmm11 : _STRUCT_XMM_REG;
          fpu_xmm12 : _STRUCT_XMM_REG;
          fpu_xmm13 : _STRUCT_XMM_REG;
          fpu_xmm14 : _STRUCT_XMM_REG;
          fpu_xmm15 : _STRUCT_XMM_REG;
          fpu_rsrv4 : array[0..(6*16)-1] of char;
          fpu_reserved1 : longint;
       end;

{$endif}
  { !__DARWIN_UNIX03  }
{$if __DARWIN_UNIX03}
  {todo: #define _STRUCT_X86_EXCEPTION_STATE64 }

  type
     __darwin_x86_exception_state64 = record
          __trapno : dword;
          __err : dword;
          __faultvaddr : __uint64_t;
       end;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define  _STRUCT_X86_EXCEPTION_STATE64 }

  type
     x86_exception_state64 = record
          trapno : dword;
          err : dword;
          faultvaddr : __uint64_t;
       end;

{$endif}
  { !__DARWIN_UNIX03  }
{$if __DARWIN_UNIX03}
  {todo: #define  _STRUCT_X86_DEBUG_STATE64 }

  type
     __darwin_x86_debug_state64 = record
          __dr0 : __uint64_t;
          __dr1 : __uint64_t;
          __dr2 : __uint64_t;
          __dr3 : __uint64_t;
          __dr4 : __uint64_t;
          __dr5 : __uint64_t;
          __dr6 : __uint64_t;
          __dr7 : __uint64_t;
       end;

{$else}
  { !__DARWIN_UNIX03  }
  {todo: #define  _STRUCT_X86_DEBUG_STATE64 }

  type
     x86_debug_state64 = record
          dr0 : __uint64_t;
          dr1 : __uint64_t;
          dr2 : __uint64_t;
          dr3 : __uint64_t;
          dr4 : __uint64_t;
          dr5 : __uint64_t;
          dr6 : __uint64_t;
          dr7 : __uint64_t;
       end;



{	File:	thread_state.h }
{* Size of maximum exported thread state in words *}
const
  I386_THREAD_STATE_MAX	= 144;    { Size of biggest state possible }

{$ifdef CPUi386}
  THREAD_STATE_MAX = I386_THREAD_STATE_MAX;
{$endif}
{$ifdef x86_64}
  THREAD_STATE_MAX = I386_THREAD_STATE_MAX;
{$endif}


  {	File:	thread_status.h
   *	Author:	Avadis Tevanian, Jr.
   *	Date:	1985
   *
   *	This file contains the structure definitions for the thread
   *	state as applied to I386 processors. }

  {* the i386_xxxx form is kept for legacy purposes since these types
   * are externally known... eventually they should be deprecated.
   * our internal implementation has moved to the following naming convention
   *
   *   x86_xxxx32 names are used to deal with 32 bit states
   *   x86_xxxx64 names are used to deal with 64 bit states
   *   x86_xxxx   names are used to deal with either 32 or 64 bit states
   *	via a self-describing mechanism }
  {* these are the legacy names which should be deprecated in the future
   * they are externally known which is the only reason we don't just get
   * rid of them }

const
  i386_THREAD_STATE     = 1;
  i386_FLOAT_STATE      = 2;
  i386_EXCEPTION_STATE  = 3;
  {* THREAD_STATE_FLAVOR_LIST 0
   * 	these are the supported flavors }
  x86_THREAD_STATE32    = 1;
  x86_FLOAT_STATE32     = 2;
  x86_EXCEPTION_STATE32 = 3;
  x86_THREAD_STATE64    = 4;
  x86_FLOAT_STATE64     = 5;
  x86_EXCEPTION_STATE64 = 6;
  x86_THREAD_STATE      = 7;
  x86_FLOAT_STATE       = 8;
  x86_EXCEPTION_STATE   = 9;
  x86_DEBUG_STATE32     = 10;
  x86_DEBUG_STATE64     = 11;
  x86_DEBUG_STATE       = 12;
  THREAD_STATE_NONE     = 13;
  {* Largest state on this machine:
   * (be sure mach/machine/thread_state.h matches!) }
  THREAD_MACHINE_STATE_MAX = THREAD_STATE_MAX;

  {* VALID_THREAD_STATE_FLAVOR is a platform specific macro that when passed
   * an exception flavor will return if that is a defined flavor for that
   * platform. The macro must be manually updated to include all of the valid
   * exception flavors as defined above.  }

function VALID_THREAD_STATE_FLAVOR(x: Integer): Boolean;


type
  x86_state_hdr = record
    flavor : longint;
    count  : longint;
  end;
  x86_state_hdr_t = x86_state_hdr;

{* Default segment register values. }

const
   USER_CODE_SELECTOR = $0017;
   USER_DATA_SELECTOR = $001f;
   KERN_CODE_SELECTOR = $0008;
   KERN_DATA_SELECTOR = $0010;


type
  i386_thread_state_t     = _STRUCT_X86_THREAD_STATE32; {* to be deprecated in the future }
  x86_thread_state32_t    = _STRUCT_X86_THREAD_STATE32;
  i386_float_state_t      = _STRUCT_X86_FLOAT_STATE32; {* to be deprecated in the future}
  x86_float_state32_t     = _STRUCT_X86_FLOAT_STATE32;
  i386_exception_state_t  = _STRUCT_X86_EXCEPTION_STATE32; {* to be deprecated in the future}
  x86_exception_state32_t = _STRUCT_X86_EXCEPTION_STATE32;

const
  i386_THREAD_STATE_COUNT    	= sizeof (i386_thread_state_t) div sizeof(integer);
  i386_FLOAT_STATE_COUNT      = sizeof (i386_float_state_t) div sizeof(longword);
  i386_EXCEPTION_STATE_COUNT  = sizeof (i386_exception_state_t) div sizeof (integer);
  x86_THREAD_STATE32_COUNT    = sizeof (x86_thread_state32_t) div sizeof (integer);
  x86_FLOAT_STATE32_COUNT     = sizeof (x86_float_state32_t) div sizeof(longword);
  x86_EXCEPTION_STATE32_COUNT	= sizeof (x86_exception_state32_t) div sizeof(integer);

  I386_EXCEPTION_STATE_COUNT = i386_EXCEPTION_STATE_COUNT;

type
  x86_debug_state32_t = _STRUCT_X86_DEBUG_STATE32;

const
  x86_DEBUG_STATE32_COUNT = sizeof(x86_debug_state32_t) div sizeof (integer);
  X86_DEBUG_STATE32_COUNT = x86_DEBUG_STATE32_COUNT;

type
  x86_thread_state64_t    = _STRUCT_X86_THREAD_STATE64;
  x86_float_state64_t     = _STRUCT_X86_FLOAT_STATE64;
  x86_exception_state64_t = _STRUCT_X86_EXCEPTION_STATE64;

const
  x86_THREAD_STATE64_COUNT = sizeof (x86_thread_state64_t) div sizeof (integer);
  x86_FLOAT_STATE64_COUNT = sizeof(x86_float_state64_t) div (sizeof(dword));
  x86_EXCEPTION_STATE64_COUNT = sizeof(x86_exception_state64_t)) div (sizeof(longint);
  X86_EXCEPTION_STATE64_COUNT = x86_EXCEPTION_STATE64_COUNT;

type
  x86_debug_state64_t = _STRUCT_X86_DEBUG_STATE64;

const
  x86_DEBUG_STATE64_COUNT	=  sizeof (x86_debug_state64_t) div sizeof (integer);
  X86_DEBUG_STATE64_COUNT = x86_DEBUG_STATE64_COUNT;

  {* Combined thread, float and exception states}

type
  x86_thread_state = record
    tsh : x86_state_hdr_t;
    uts : record case longint of
          0 : ( ts32 : x86_thread_state32_t );
          1 : ( ts64 : x86_thread_state64_t );
          end;
  end;

  x86_float_state = record
    fsh : x86_state_hdr_t;
    ufs : record case longint of
          0 : ( fs32 : x86_float_state32_t );
          1 : ( fs64 : x86_float_state64_t );
          end;
  end;

  x86_exception_state = record
    esh : x86_state_hdr_t;
    ues : record case longint of
          0 : ( es32 : x86_exception_state32_t );
          1 : ( es64 : x86_exception_state64_t );
          end;
  end;

  x86_debug_state = record
    dsh : x86_state_hdr_t;
    uds : record case longint of
          0 : ( ds32 : x86_debug_state32_t );
          1 : ( ds64 : x86_debug_state64_t );
          end;
  end;

  x86_thread_state = x86_thread_state_t;
  x86_float_state = x86_float_state_t;
  x86_exception_state = x86_exception_state_t;
  x86_debug_state = x86_debug_state_t;

type
  x86_THREAD_STATE_COUNT    = sizeof (x86_thread_state_t) div sizeof (integer);
  x86_FLOAT_STATE_COUNT     = sizeof(x86_float_state_t) div sizeof(longwordinteger);
  x86_EXCEPTION_STATE_COUNT = sizeof(x86_exception_state_t) div sizeof(longword);
  x86_DEBUG_STATE_COUNT    = sizeof(x86_debug_state_t) div sizeof(longword);

  {* Machine-independent way for servers and Mach's exception mechanism to
   * choose the most efficient state flavor for exception RPC's:  }

const
  {$ifdef i386}
  MACHINE_THREAD_STATE = x86_THREAD_STATE;
  MACHINE_THREAD_STATE_COUNT = x86_THREAD_STATE_COUNT;
  {$endif}

  {* when reloading the segment registers on
   * a return out of the kernel, we may take
   * a GeneralProtection or SegmentNotPresent
   * fault if one or more of the segment
   * registers in the saved state was improperly
   * specified via an x86_THREAD_STATE32 call
   * the frame we push on top of the existing
   * save area looks like this... we need to
   * carry this as part of the save area
   * in case we get hit so that we have a big
   * enough stack
    }

type
  x86_seg_load_fault32 = record
    trapno  : dword;
    err     : dword;
    eip     : dword;
    cs      : dword;
    efl     : dword;
  end;


implementation

function VALID_THREAD_STATE_FLAVOR(x: Integer): Boolean;
begin
  case x of
  x86_THREAD_STATE32  x86_FLOAT_STATE32,  x86_EXCEPTION_STATE32, x86_DEBUG_STATE32,
  x86_THREAD_STATE64, x86_FLOAT_STATE64,  x86_EXCEPTION_STATE64, x86_DEBUG_STATE64,
  x86_THREAD_STATE,   x86_FLOAT_STATE, 	  x86_EXCEPTION_STATE,   x86_DEBUG_STATE,
  THREAD_STATE_NONE:
    Result := true;
  else
    Result := false;
  end;
end;

end.

