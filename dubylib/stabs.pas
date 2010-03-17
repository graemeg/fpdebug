unit stabs;

{$mode objfpc}{$H+}

// STABS specification http://www.cygwin.com/stabs.html

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
  N_EXTTYPE  = N_TYPE or N_EXT;
  N_PEXTTYPE = N_TYPE or N_PEXT;
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

// The overall format of the string field for most stab types is:
//  "name:symbol-descriptor type-information"

// "name" is the name of the symbol represented by the stab; it can contain
// a pair of colons (see section Defining a Symbol Within Another Type). name
// can be omitted, which means the stab represents an unnamed object.
//   For example, `:t10=*2' defines type 10 as a pointer to type 2, but does
// not give the type a name. GCC sometimes uses a single space as the name
// instead of omitting the name altogether.

// The "symbol-descriptor" following the `:' is an alphabetic character that
// tells more specifically what kind of symbol the stab represents. If the symbol-descriptor
// is omitted, but type information follows, then the stab represents a local variable.
// For a list of symbol descriptors, see section Table of Symbol Descriptors. The `c' symbol
// descriptor is an exception in that it is not followed by type information. See section Constants.

// type-information is either a type-number, or `type-number='. A type-number alone is a type reference,
// referring directly to a type that has already been defined.

// The `type-number=' form is a type definition, where the number represents a new type which
// is about to be defined. The type definition may refer to other types by number, and those
// type numbers may be followed by `=' and nested definitions. Also, the Lucid compiler will
// repeat `type-number=' more than once if it wants to define several type numbers at once.

procedure ParseStabStr(const str: string; var name, desc: string; var typeNum: Integer; var value: string);
procedure StabVarStr(const varstr: String; var name, descr: string; var vartype: Integer);
procedure StabFuncStr(const funcstr: String; var name: string);


// The symbol descriptor is the character which follows the colon in many stabs,
// and which tells what kind of stab it is. See section The String Field, for more information about their use.

const
  Sym_ParamByRefInReg = 'a'; // Parameter passed by reference in register; see section Passing Parameters by Reference.
  Sym_BasedVar        = 'b'; // Cased variable; see section Fortran Based Variables.
  Sym_Constant        = 'c'; // Constant; see section Constants.
  Sym_ArrayBound      = 'C'; // Conformant array bound (Pascal, maybe other languages); section Passing Conformant Array Parameters. Name of a caught exception (GNU C++). These can be distinguished because the latter uses N_CATCH and the former uses another symbol type.
  Sym_FloatPointVar   = 'd'; // Floating point register variable; see section Register Variables.
  Sym_FloatPointReg   = 'D'; // Parameter in floating point register; see section Passing Parameters in Registers.
  Sym_FileScopeFunc   = 'f'; // File scope function; see section Procedures.
  Sym_GlobalFunc      = 'F'; // Global function; see section Procedures.
  Sym_GlobalVar       = 'G'; // Global variable; see section Global Variables.
  // 'i' See section Passing Parameters in Registers.
  Sym_NestedProc    = 'I'; // Internal (nested) procedure; see section Nested Procedures.
  Sym_NesterFunc    = 'J'; // Internal (nested) function; see section Nested Procedures.
  Sym_Label_AIX     = 'L'; // Label name (documented by AIX, no further information known).
  Sym_Module        = 'm'; // Module; see section Procedures.
  Sym_Parameter     = 'p'; //Argument list parameter; see section Parameters.
  // pP See section Parameters.
  Sym_FuncParam     = 'pF'; // Fortran Function parameter; see section Parameters.

  Sym_GlobalProc_AIX = 'p'; // Unfortunately, three separate meanings have been independently invented for this symbol
  Sym_RegParam_GNU   = 'p'; // descriptor. At least the GNU and Sun uses can be distinguished by the symbol type.
  Sym_ProtoType_SUN  = 'p'; // Global Procedure (AIX) (symbol type used unknown); see section Procedures.
                            // Register parameter (GNU) (symbol type N_PSYM); see section Parameters.
                            // Prototype of function referenced by this file (Sun acc) (symbol type N_FUN).

  Sym_StaticProc    = 'Q';  // Static Procedure; see section Procedures.
  Sym_ParamInReg    = 'R';  // Register parameter; see section Passing Parameters in Registers.
  Sym_VarInReg      = 'r';  // Register variable; see section Register Variables.
  Sym_FileScopeVar  = 'S';  // File scope variable; see section Static Variables.
  Sym_LocalVar_OS9K = 's';  // Local variable (OS9000).
  Sym_TypeName      = 't';  // Type name; see section Giving a Type a Name.
  Sym_StructType    = 'T';  // Enumeration, structure, or union tag; see section Giving a Type a Name.
  Sym_ParamByRef    = 'v';  // Parameter passed by reference; see section Passing Parameters by Reference.
  Sym_StaticVar     = 'V';  // Procedure scope static variable; see section Static Variables.
  Sym_ConformArray  = 'x';  // Conformant array; see section Passing Conformant Array Parameters.
  Sym_ReturnVar     = 'X';  // Function return variable; see section Parameters.


// The type descriptor is the character which follows the type number and an equals sign.
// It specifies what kind of type is being defined. See section The String Field, for
// more information about their use.

const
  SymType_BuiltInType = '-'; //Reference to builtin type; see section Negative Type Numbers.
  SymType_CppMethod   = '#'; // Method (C++); see section The `#' Type Descriptor.
  SymType_Pointer     = '*'; // Pointer; see section Miscellaneous Types.
  SymType_CppRef      = '&'; // Reference (C++).

  SymType_Attribute_AIX = '@'; // Type Attributes (AIX); see section The String Field.
  SymType_Mebmer_GNU    = '@'; // Member (class and variable) type (GNU C++); see section The `@' Type Descriptor.

  SymType_Array       = 'a'; // Array; see section Array Types.
  SymType_OpenArray   = 'A'; // Open array; see section Array Types.

  SymType_PasSpace_AIX  = 'b'; // Pascal space type (AIX); see section Miscellaneous Types.
  SymType_Integer_SUN   = 'b'; // Builtin integer type (Sun); see section Defining Builtin Types Using Builtin Type Descriptors.
  SYmType_Volatile_OS9k = 'b'; //Const and volatile qualfied type (OS9000).

  SymType_Volatile    = 'B'; // Volatile-qualified type; see section Miscellaneous Types.

  SymType_Complex_AIX = 'c'; // Complex builtin type (AIX); see section Defining Builtin Types Using Builtin Type Descriptors.
  SymType_Const_OS9k  = 'c'; // Const-qualified type (OS9000).

  SymType_File        = 'd'; // File type; see section Miscellaneous Types.
  SymType_DimArray    = 'D'; // N-dimensional dynamic array; see section Array Types.
  SymType_Enum        = 'e'; // Enumeration type; see section Enumerations.
  SymType_DimSubArray = 'E'; // N-dimensional subarray; see section Array Types.
  SymType_Func        = 'f'; // Function type; see section Function Types.
  SymType_PasFunc     = 'F'; // Pascal function parameter; see section Function Types
  SymType_Float       = 'g'; // Builtin floating point type; see section Defining Builtin Types Using Builtin Type Descriptors.

  SymType_Imported_AIX   = 'i'; // Imported type (AIX); see section Cross-References to Other Types.
  SymType_Volatile2_OS9k = 'i'; // Volatile-qualified type (OS9000).

  SymType_ConstQual   = 'k'; // Const-qualified type; see section Miscellaneous Types.

  SymType_CobolPict   = 'C'; // COBOL Picture type. See AIX documentation for details.
  SymType_CobolGroup  = 'G'; // COBOL Group. See AIX documentation for details.
  SymType_CobolFile   = 'K'; // COBOL File Descriptor. See AIX documentation for details.

  SymType_Multiple    = 'M'; // Multiple instance type; see section Miscellaneous Types.
  SymType_String      = 'n'; // String type; see section Strings.
  SymType_StringPtr   = 'N'; // Stringptr; see section Strings.
  SymType_Opaque      = 'o'; // Opaque type; see section Giving a Type a Name.
  SymType_Procedure   = 'p'; // Procedure; see section Function Types.
  SymType_PackedArray = 'P'; // Packed array; see section Array Types.
  SymType_Range       = 'r'; // Range type; see section Subrange Types.

  SymType_Float2_Sun    = 'R'; // Builtin floating type; see section Defining Builtin Types Using Builtin Type Descriptors (Sun).
  SymType_PascalSub_AIX = 'R'; // Pascal subroutine parameter; see section Function Types (AIX).
                               // Detecting this conflict is possible with careful parsing
                               // (hint: a Pascal subroutine parameter type will always contain a comma,
                               // and a builtin type descriptor never will).

  SymType_Struct     = 's'; // Structure type; see section Structures.
  SymType_Set        = 'S'; // Set type; see section Miscellaneous Types.
  SymType_Union      = 'u'; // Union; see section Unions.
  SymType_Variant    = 'v'; // Variant record. This is a Pascal and Modula-2 feature which is like
                            // a union within a struct in C. See AIX documentation for details.

  SymType_Widechar   = 'w'; // Wide character; see section Defining Builtin Types Using Builtin Type Descriptors.
  SymType_CrossRef   = 'x'; // Cross-reference; see section Cross-References to Other Types.
  SymType_Struct_IBM = 'Y'; // Used by IBM's xlC C++ compiler (for structures, I think).
  SymType_gstring    = 'z'; // gstring; see section Strings.

const
  bitype_SInt32int  = -1;  // int, 32 bit signed integral type.
  bitype_SInt8char  = -2;  // char, 8 bit type holding a character. Both GDB and dbx on AIX treat this as signed.
                           // GCC uses this type whether char is signed or not, which seems like a bad idea.
                           // The AIX compiler (xlc) seems to avoid this type; it uses -5 instead for char.
  bitype_SInt16     = -3;  // short, 16 bit signed integral type.
  bitype_SInt32long = -4;  // long, 32 bit signed integral type.
  bitype_UInt8      = -5;  // unsigned char, 8 bit unsigned integral type.
  bitype_SInt8      = -6;  // signed char, 8 bit signed integral type.
  bitype_UInt16     = -7;  // unsigned short, 16 bit unsigned integral type.
  bitype_UInt32int  = -8;  // unsigned int, 32 bit unsigned integral type.
  bitype_UInt32     = -9;  // unsigned, 32 bit unsigned integral type.
  bitype_UInt32long = -10; // unsigned long, 32 bit unsigned integral type.
  bitype_Void       = -11; // void, type indicating the lack of a value.
  bitype_Single     = -12; // float, IEEE single precision.
  bitype_Double     = -13; // double, IEEE double precision.
  bitype_Extended   = -14; // long double, IEEE double precision. The compiler claims the size will
                           // increase in a future release, and for binary compatibility you have to avoid using
                           // long double. I hope when they increase it they use a new negative type number.
  bitype_SInt32integer = -15; // integer. 32 bit signed integral type.
  bitype_Boolean32     = -16; // boolean. 32 bit type. GDB and GCC assume that zero is false, one is true, and other values have unspecified meaning. I hope this agrees with how the IBM tools use the type.
  bitype_ShortReal     = -17; // short real. IEEE single precision.
  bitype_Real          = -18; // real. IEEE double precision.
  bitype_StringPtr     = -19; // stringptr. See section Strings.
  bitype_Charcter      = -20; // character, 8 bit unsigned character type.
{-21; // logical*1, 8 bit type. This Fortran type has a split personality in that it is used for boolean variables, but can also be used for unsigned integers. 0 is false, 1 is true, and other values are non-boolean.
-22; // logical*2, 16 bit type. This Fortran type has a split personality in that it is used for boolean variables, but can also be used for unsigned integers. 0 is false, 1 is true, and other values are non-boolean.
-23; // logical*4, 32 bit type. This Fortran type has a split personality in that it is used for boolean variables, but can also be used for unsigned integers. 0 is false, 1 is true, and other values are non-boolean.
-24; // logical, 32 bit type. This Fortran type has a split personality in that it is used for boolean variables, but can also be used for unsigned integers. 0 is false, 1 is true, and other values are non-boolean.
}
  bitype_2Singles   = -25; // complex. A complex type consisting of two IEEE single-precision floating point values.
  bitype_2Doubles   = -26; // complex. A complex type consisting of two IEEE double-precision floating point values.
  bitype_SInt8int1  = -27; // integer*1, 8 bit signed integral type.
  bitype_SInt16int2 = -28; // integer*2, 16 bit signed integral type.
  bitype_SInt32int4 = -29; // integer*4, 32 bit signed integral type.
  bitype_wchar      = -30; // wchar. Wide character, 16 bits wide, unsigned (what format? Unicode?).
  bitype_SInt64     = -31; // long long, 64 bit signed integral type.
  bitype_UInt64     = -32; // unsigned long long, 64 bit unsigned integral type.
  bitype_UInt64log8 = -33; // logical*8, 64 bit unsigned integral type.
  bitype_SInt64log8 = -34; // integer*8, 64 bit signed integral type.


type
  TStabType = packed record
    _typestr  : string;
    _typenum  : Integer;
  end;

function isArrayType(const value: string; index: integer): Boolean;

function isStructType(const value: string; var StructSize: Integer; var firstElemIndex: Integer): Boolean;
function GetStructElem(const s: string; index: Integer; var Name, TypeStr: string; var BitsOffset, BitsSize: Integer; var nextElemIndex: Integer): Boolean;

type
  TStabReadCallback = procedure (AType, Misc: Byte; Desc: Word; Value: LongWord; const StabStr: String) of object;

procedure ReadStabSyms(const StabsBuf, StabStrBuf : array of Byte;
  StabsCount, StabStrLen: Integer; Callback: TStabReadCallback);

function isProcArgument(const VarDesc: String): Boolean;

//array type
function isSymTypeArray(const typeval: string): Boolean;

// GetArrayIndexTypeRange returns array index range type declaration         //
// and the starting index of number elements type.                           //
// i.e.:                                                                     //
// input:                                                                    //
//   arrtypeval: ar7;0;5;6                                                   //
// output:                                                                   //
//   Result:    true                                                         //
//   RangeVal:  r7;0;5;                                                      //
//   elemIndex: 9 (the index of '6');                                        //
//   isPacked:  false                                                        //
// if non array uses non range type for indexes, the function return false!  //
function GetArrayIndexTypeRange(const arrtypeval: string; var RangeVal: string; var elemIndex: Integer; var isPacked: Boolean): Boolean;

//range type
function isSymTypeRange(const typeval: string): Boolean;
function ParseSymTypeSubRangeVal(const v: String; var TypeNum: Integer; var LowerRange, HighRange: String): Boolean;

// struct type
function isSymTypeStruct(const TypeVal: string): Boolean;
function ParseStructSize(const v: String; var StructBytes: Integer; var FirstElemIndex: Integer): Boolean;
function NextStructElem(const v: String; Index: Integer; var ElemName : string; var TypeDeclIndex: Integer): Boolean;
function NextStructElemPos(const v: String; Index: Integer; var BitOfs, BitSize: Integer; var NextElemIndex: Integer): Boolean;

// enum type
function isSymTypeEnum(const TypeVal: String): Boolean;
// the function reads the next enumeration declaration.             //
// note, the input string must not contain 'e' (enum type modifier) //
// input:                                                           //
//   s:     E1:0,E2:1,                                              //
//   index: 1                                                       //
// output:                                                          //
//   index: 6                                                       //
//   Name:  E1                                                      //
//   Value: 0                                                       //
function NextEnumVal(const s: string; var index: Integer; var Name, Value: string): Boolean;

//some simple type (pointer, void, aliases)
function ParseSymTypeVal(const v: STring; var TypeNum: Integer; var TypeDescr: String): Boolean;

// parses for the next value attribute (see The String Field) for
// Each one starts with `@' and ends with `;'
function NextValAttr(const v: String; var index: Integer; var attr: String): Boolean;
// returns the size of bits, set by the attribute.
// attr string must NOT start with @ symbol
function GetBitSizeAttr(const attr: string; var BitSize: Integer): Boolean;

implementation

const
  NumChars    = ['0'..'9'];
  SymChars    = ['+','-'];
  SymNumChars = SymChars+NumChars;

function GetNextNumber(const s: String; var index: Integer; var numStr: string): Boolean;
var
  j : Integer;
begin
  Result:=(index>=1) and (index<=length(s)) and (s[index] in SymNumChars);
  if not Result then Exit;
  j:=index;
  if s[j] in SymChars then inc(j);
  Result:=(j<=length(s)) and (s[j] in NumChars);
  if not Result then Exit;
  while (j<=length(s)) and (s[j] in NumChars) do inc(j);
  numStr:=copy(s, index, j-index);
  index:=j;
end;

function isSymTypeArray(const typeval: string): Boolean;
begin
  Result:=(length(typeval)>0) and
          ((typeval[1]=SymType_Array) or
          (typeval[1]=SymType_PackedArray));
end;

function GetArrayIndexTypeRange(const arrtypeval: string; var RangeVal: string; var elemIndex: Integer;
  var isPacked: Boolean): Boolean;
var
  i : Integer;
  cnt : Integer;
begin
  Result:=(length(arrtypeval)>3) and isSymTypeArray(arrtypeval) and (arrtypeval[2]=SymType_Range);
  if not Result then Exit;

  isPacked:=arrtypeval[1]=SymType_PackedArray;
  i:=2;
  cnt:=0;
  for i:=2 to length(arrtypeval) do
    if arrtypeval[i]=';' then begin
      inc(cnt);
      if cnt=3 then begin
        elemIndex:=i+1;
        RangeVal:=Copy(arrtypeval, 2, elemIndex-2);
        Result:=True;
        Exit;
      end;
    end;
  Result:=false;
end;

function isSymTypeRange(const typeval: string): Boolean;
begin
  Result:=(length(typeval)>0) and (typeval[1]=SymType_Range);
end;

function isSymTypeStruct(const TypeVal: string): Boolean;
begin
  Result:=(length(typeval)>0) and (typeval[1]=SymType_Struct);
end;

function ParseStructSize(const v: String; var StructBytes: Integer; var FirstElemIndex: Integer): Boolean;
var
  i   : Integer;
  nm  : String;
  err : Integer;
begin
  Result:=isSymTypeStruct(v) and (length(v)>1);
  if not Result then Exit;

  i:=2;
  GetNextNumber(v, i, nm);
  Val(nm, StructBytes, err);
  Result:=err=0;
  FirstElemIndex:=i;
end;

function NextStructElem(const v: String; Index: Integer; var ElemName : string; var TypeDeclIndex: Integer): Boolean;
var
  i   : Integer;
  j   : Integer;
  nm  : String;
  err : Integer;
begin
  Result:=(Index>=1) and (Index<=length(v));
  if not Result then Exit;
  j:=0;
  for i:=Index to length(v) do
    if v[i]=':' then begin
      ElemName:=Copy(v, index, i-index);
      TypeDeclIndex:=i+1;
      Result:=True;
      Exit;
    end;
  Result:=False;
end;

function NextStructElemPos(const v: String; Index: Integer; var BitOfs, BitSize: Integer; var NextElemIndex: Integer): Boolean;
var
  i   : Integer;
  nm  : String;
  err : Integer;
begin
  Result:=(Index>=1) and (Index<=length(v));
  if not Result then Exit;

  if v[Index]=',' then inc(Index);
  i:=Index;
  GetNextNumber(v, i, nm);
  Val(nm, BitOfs, err);
  Result:=err=0;
  if not Result then Exit;
  inc(i); // skipping ","

  GetNextNumber(v, i, nm);
  Val(nm, BitSize, err);
  Result:=err=0;
  if not Result then Exit;
  inc(i); // skipping ";"

  NextElemIndex:=i;
end;

function isSymTypeEnum(const TypeVal: String): Boolean;
begin
  Result:=(length(TypeVal)>0) and (typeVal[1]=SymType_Enum);
end;

function NextEnumVal(const s: string; var index: Integer; var Name, Value: string): Boolean;
var
  i,j : Integer;
begin
  if (index<=0) or (index>length(s)) or (s[index]=';') then begin
    Result:=false;
    Exit;
  end;
  j:=index;
  for i:=index to length(s) do begin
    if s[i]=':' then begin
      j:=i+1;
      Name:=Copy(s, index, i - index);
    end;
    if s[i]=',' then begin
      Value:=Copy(s, j, i - j);
      Result:=True;
      index:=i+1;
      Exit;
    end;
  end;
  Result:=False;
  index:=length(s)+1;
end;

function ParseSymTypeSubRangeVal(const v: String; var TypeNum: Integer; var LowerRange, HighRange: String): Boolean;
var
  i   : Integer;
  nm  : String;
  err : Integer;
begin
  i:=2; // skip the first "r"
  GetNextNumber(v, i, nm);
  Val(nm, TypeNum, err);
  Result:=err=0;
  inc(i); // skip ";"

  Result:=Result and GetNextNumber(v, i, LowerRange);
  inc(i); // skip ";"
  Result:=Result and GetNextNumber(v, i, HighRange);
  inc(i); // skip ";"
end;

function ParseSymTypeVal(const v: String; var TypeNum: Integer; var TypeDescr: String): Boolean;
var
  i   : Integer;
  err : Integer;
  nm  : string;
begin
  Result:=length(v)>0;
  if not Result then Exit;
  i:=1;
  if not (v[i] in ['-','0'..'9']) then begin
    TypeDescr:=v[i];
    inc(i);
  end else
    TypeDescr:='';
  GetNextNumber(v, i, nm);
  Val(nm, TypeNum, err);
  Result:=err=0;
end;


function isProcArgument(const VarDesc: String): Boolean;
begin
  Result:=(Pos(Sym_ParamInReg, VarDesc)>0) or (Pos(Sym_Parameter, VarDesc)>0)
end;

const 
  NameSeparator = ':';

  Numbers       = ['0'..'9'];
  NumbersAndNeg = ['-'] + Numbers;

function GetSubStr(const s: string; Index: Integer; Sep: AnsiChar): string;
var
  i : integer;
begin
  for i := Index to length(s) do
    if s[i] = Sep then begin
      Result := Copy(s, Index, i - Index);
      Exit;
    end;
  Result := Copy(s, Index, length(s) - Index+1);
end;

function GetNextName(const s: string; index: Integer): string; inline;
begin
  Result := GetSubStr(s, index, NameSeparator);
end;

function GetNextNumber(const s: string; index: Integer; var Number: Int64; var NumLen: Integer): Boolean; overload;
var
  num : string;
  i   : Integer;
  err : Integer;
  numstop : Boolean;
begin
  numlen := 0;
  Number := 0;
  Result := (s<>'') and (index>=1) and (index<=length(s)) and (s[index] in NumbersAndNeg);
  if not Result then Exit;

  if s[index] = '-' then begin
    num := '-';
    inc(index);
  end else
    num := '';

  numstop := false;
  for i := index to length(s) do
    if not (s[i] in Numbers) then begin
      numstop := true;
      numlen := i - index;
      num := num + Copy(s, index, numlen);
      Break;
    end;

  if not numstop then begin
    numlen := length(s) - index+1;
    num := num + Copy(s, index, numlen);
  end;

  Val(num, Number, err);
  Result := err <> 0;
end;

function GetNextNumber(const s: string; index: Integer; var Number: Integer; var NumLen: Integer): Boolean; overload;
var
  n   : Int64;
begin
  Result := GetNextNumber(s, index, n, NumLen);
  Number := n;
end;

function GetTypeString(const s: String; index: Integer): String; forward;

function GetArrayTypeString(const s: string; index: Integer): String;
var
  i, j  : integer;
  semi  : Integer;
begin
  Result := '';
  if not isArrayType(s, index) then Exit;
  i := index+1;
  if (s[i] = SymType_PackedArray) or (s[i] = SymType_Range) then begin
    semi := 3;
    inc(i);
  end;

  for i := i to length(s) do
    if s[i] = ';' then begin
      dec(semi);
      j := i+1;
      Result := Copy(s, index, j - index);
      if semi = 0 then Break;
    end;
  if semi > 0 then
    Result := Copy(s, index, length(s)-index+1)
  else begin
    Result := Result + GetTypeString(s, j);
  end;
end;

function GetStructElem(const s: string; index: Integer; var Name, TypeStr: string; var BitsOffset, BitsSize: Integer; var nextElemIndex: Integer): Boolean;
var
  len : integer;
begin
  Name := GetNextName(s, index);
  if Name = ';' then Name := '';
  Result := Name <> '';
  if not Result then Exit;

  index := index + length(name)+1;

  TypeStr := GetTypeString(s, index);
  inc(index, length(TypeStr)+1);

  GetNextNumber(s, index, BitsOffset, len);
  inc(index,len+1);

  GetNextNumber(s, index, BitsSize, len);

  inc(index,len+1);
  nextElemIndex := index;
  Result := true;
end;

procedure ReadStabSyms(const StabsBuf,StabStrBuf: array of Byte; StabsCount,
  StabStrLen:Integer;Callback:TStabReadCallback);
type
  TStabsArray = array [byte] of TStabSym;
  PStabsArray = ^TStabsArray;
var
  stabs : PStabsArray;
  str   : PChar;
  cnt   : Integer;
  i     : Integer;
begin
  if not ASsigned(Callback) then Exit;

  stabs:=@StabsBuf;
  for i:=0 to StabsCount-1 do
    Callback(
      stabs^[i].n_type, stabs^[i].n_other, stabs^[i].n_desc, stabs^[i].n_value,
      //todo: make check
      PChar(@StabStrBuf[stabs^[i].n_strx])
    );
end;

function GetTypeString(const s: String; index: Integer): String;
begin
  if s[index] in NumbersAndNeg then begin
    Result  := GetSubStr(s, index, ',');
  end else if isArrayType( s, index ) then begin
    Result := GetArrayTypeString(s, index);
  end;
  //todo: ANY OTHER TYPES?
end;

function isArrayType(const value: string; index: integer): Boolean;
begin
  Result := (value<>'') and (index>=1) and (index<=length(value)) and (value[index] = SymType_Array);
end;

function isStructType(const value: string; var StructSize: Integer; var firstElemIndex: Integer): Boolean;
var
  len : Integer;
  n   : Int64;
begin
  Result := (value <> '') and (value[1] = SymType_Struct);
  if not Result then Exit;

  GetNextNumber(value, 2, n, len);
  StructSize := n;
  firstElemIndex := 2+len;
end;


procedure ParseDescr(const descStr: String; var desc: string; var descNum: Integer);
var
  i   : integer;
  err : integer;
begin
  for i := 1 to length(descStr) do
    if descStr[i] in ['-','0'..'9'] then begin
      Val( Copy(descStr, i, length(descStr)-i+1), descNum, err);
      desc := Copy(descStr, 1, i-1);
      Exit;
    end;
  descNum := 0;
  desc:=descStr;
end;

procedure ParseStabStr(const str: string; var name, desc: string; var typeNum: Integer; var value: string);
var
  i, j : integer;
  k    : integer;
  m    : string;
begin
  desc := '';
  typeNum := 0;
  value := '';

  name := GetNextName(str, 1);
  j := length(name)+1;
  if j > length(str) then
    Exit
  else if str[j]=NameSeparator then
    inc(j);

  k:=-1;
  for i := j to length(str) do
    if str[i] = '=' then begin
      m := Copy(str, j, i - j);
      k :=i+1;
      Break
    end;
  if k < 0 then
    m := Copy(str, j, length(str) - j+1)
  else
    value := Copy(str, k, length(str)-k+1);

  ParseDescr(m, desc, typeNum);
end;

procedure StabVarStr(const varstr: String; var name, descr: string; var vartype: Integer);
var
  value   : String;
begin
  ParseStabStr(varstr, name, descr, varType, value);
end;

procedure StabFuncStr(const funcstr: String; var name: string);
var
  md, value : string;
  nmd       : Integer;
begin
  ParseStabStr(funcstr, name, md, nmd, value);
end;

function NextValAttr(const v: String; var index: Integer; var attr: String): Boolean;
var
  i : Integer;
begin
  Result:=(index>=0) and (index<=length(v)) and (v[index]=SymType_Attribute_AIX);
  if not Result then Exit;
  inc(index);
  for i:=index to length(v) do
    if (v[i]=SymType_Attribute_AIX) or (v[i]=';') then begin
      attr:=Copy(v, index, i-index);
      index:=i;
      if v[i]=';' then inc(index);
      Result:=True;
      Exit;
    end;
  attr:=Copy(v, index, length(v)-index+1);
  index:=length(v)+1;
  Result:=True;
end;

function GetBitSizeAttr(const attr: string; var BitSize: Integer): Boolean;
var
  err : Integer;
begin
  Result:=(length(attr)>=1) and (attr[1]='s');
  if Result then begin
    Val(Copy(attr, 2, length(attr)-1), BitSize, err);
    Result:=err=0;
  end;
end;


end.

