unit stabs;

{$mode objfpc}{$H+}

interface

const
  //StabSectoinName
  _stab    = '.stab';
  _stabstr = '.stabstr';

const
  //Non-Stab Symbol Types
  //The following types are used by the linker and assembler, not by stab directives.
  //Since this document does not attempt to describe aspects of object file format
  //other than the debugging format, no details are given.

  N_UNDF     = $00;  // Undefined symbol
  N_EXT      = $01;  // External modifier

  N_ABS      = $02;  // File scope absolute symbol
  N_ABS_EXT  = N_ABS or N_EXT;  // External absolute symbol

  N_TEXT     = $04;  // File scope text symbol
  N_TEXT_EXT = N_TEXT or N_EXT; // External text symbol

  N_DATA     = $06; // File scope data symbol
  N_DATA_EXT = $07; // External data symbol

  N_BSS      = $08; // File scope BSS symbol
  N_BSS_EXT  = N_BSS or N_EXT; // External BSS symbol

  N_FN_SEQ   = $0c; // Same as N_FN, for Sequent compilers
  N_INDR     = $0a; // Symbol is indirected to another symbol
  N_COMM     = $12; // Common--visible after shared library dynamic link

  N_SETA     = $14; //Absolute set element
  N_SETA_EXT = N_SETA or N_EXT;

  N_SETT     = $16; // Text segment set element
  N_SETT_EXT = N_SETT or N_EXT;

  N_SETD     = $18;  // Data segment set element
  N_SETD_EXT = N_SETD or N_EXT;

  N_SETB     = $1a; // BSS segment set element
  N_SETB_EXT = N_SETB or N_EXT;

  N_SETV     = $1c; // Pointer to set vector
  N_SETV_EXT = N_SETV or N_EXT;

  N_WARNING  = $1e; // Print a warning message during linking
  N_FN       = $1f; // File name of a `.o' file

const
  //Stab Symbol Types
  //The following symbol types indicate that this is a stab. This is the full
  //list of stab numbers, including stab types that are used in languages
  //other than C.

  N_GSYM   = $20; // Global symbol; see section Global Variables.
  N_FNAME  = $22; // Function name (for BSD Fortran); see section Procedures.
  N_FUN    = $24; // Function name (see section Procedures) or text segment variable (see section Static Variables).
  N_STSYM  = $26; // Data segment file-scope variable; see section Static Variables.
  N_LCSYM  = $28; // BSS segment file-scope variable; see section Static Variables.
  N_MAIN   = $2a; // Name of main routine; see section Main Program.
  N_ROSYM  = $2c; // Variable in .rodata section; see section Static Variables.
  N_PC     = $30; // Global symbol (for Pascal); see section N_PC.
  N_NSYMS  = $32; // Number of symbols (according to Ultrix V4.0); see section N_NSYMS.
  N_NOMAP  = $34; // No DST map; see section N_NOMAP.
  N_OBJ    = $38; // Object file (Solaris2).
  N_OPT    = $3c; // Debugger options (Solaris2).
  N_RSYM   = $40; // Register variable; see section Register Variables.
  N_M2C    = $42; // Modula-2 compilation unit; see section N_M2C.
  N_SLINE  = $44; // Line number in text segment; see section Line Numbers.
  N_DSLINE = $46; // Line number in data segment; see section Line Numbers.
  N_BSLINE = $48; // Line number in bss segment; see section Line Numbers.
  N_BROWS  = $48; // Sun source code browser, path to `.cb' file; see section N_BROWS.
  N_DEFD   = $4a; // GNU Modula2 definition module dependency; see section N_DEFD.
  N_FLINE  = $4c; // Function start/body/end line numbers (Solaris2).
  N_EHDECL = $50; // GNU C++ exception variable; see section N_EHDECL.
  N_MOD2   = $50; // Modula2 info "for imc" (according to Ultrix V4.0); see section N_MOD2.
  N_CATCH  = $54; // GNU C++ catch clause; see section N_CATCH.
  N_SSYM   = $60; // Structure of union element; see section N_SSYM.
  N_ENDM   = $62; // Last stab for module (Solaris2).
  N_SO     = $64; // Path and name of source file; see section Paths and Names of the Source Files.
  N_LSYM   = $80; // Stack variable (see section Automatic Variables Allocated on the Stack) or type (see section Giving a Type a Name).
  N_BINCL  = $82; // Beginning of an include file (Sun only); see section Names of Include Files.
  N_SOL    = $84; // Name of include file; see section Names of Include Files.
  N_PSYM   = $a0; // Parameter variable; see section Parameters.
  N_EINCL  = $a2; // End of an include file; see section Names of Include Files.
  N_ENTRY  = $a4; // Alternate entry point; see section Alternate Entry Points.
  N_LBRAC  = $c0; // Beginning of a lexical block; see section Block Structure.
  N_EXCL   = $c2; // Place holder for a deleted include file; see section Names of Include Files.
  N_SCOPE  = $c4; // Modula2 scope information (Sun linker); see section N_SCOPE.
  N_RBRAC  = $e0; // End of a lexical block; see section Block Structure.
  N_BCOMM  = $e2; // Begin named common block; see section Common Blocks.
  N_ECOMM  = $e4; // End named common block; see section Common Blocks.
  N_ECOML  = $e8; // Member of a common block; see section Common Blocks.
  N_WITH   = $ea; // Pascal with statement: type,,0,0,offset (Solaris2).
  N_NBTEXT = $f0; // Gould non-base registers; see section Non-base registers on Gould systems.
  N_NBDATA = $f2; // Gould non-base registers; see section Non-base registers on Gould systems.
  N_NBBSS  = $f4; // Gould non-base registers; see section Non-base registers on Gould systems.
  N_NBSTS  = $f6; // Gould non-base registers; see section Non-base registers on Gould systems.
  N_NBLCS  = $f8; // Gould non-base registers; see section Non-base registers on Gould systems.
  
const
  {darwin (mach-o) special, see: /usr/include/mach-o/nlist.h }
  
	N_STAB = $e0;  { if any of these bits set, a symbolic debugging entry }
	N_PEXT = $10;  { private external symbol bit }
	N_TYPE = $0e;  { mask for the type bits }
	//N_EXT	 = $01;  { external symbol bit, set for external symbols }
  
type
  bfd_vma = LongWord;
  TStabSym = packed record
    n_strx  : LongWord;  { index into string table of name }
    n_type  : Byte;      { type of symbol }
    n_other : Byte;      { misc info (usually empty) }
    n_desc  : Word;      { description field }
    n_value : bfd_vma;   { value of symbol }
  end;

implementation

end.

