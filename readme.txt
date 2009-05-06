hello

this project is my graduation work. So, generally i'm the most interested in getting this project done :)

the second, and probably even more important task, is to implement FreePascal based debugger, so it can be
used by any object-pascal (FreePascal) based application.

whole code will be written in {$mode delphi}, so, at least, windows version should be delphi 7 compatible.
i will soon upload source file.

the project is inspired, by fpdbg - simple 64-bit Win debugger by Marc Weustnik (fpc/lazarus team developer)
you can find his project at Lazarus/debugger


thanks,
dmitry aka skalogryz


=== Notes ===

the project structures is the following

fpdbglib/    - dir containg all libraries used for 
  dbg*       - common debugging unit. Usually contains classes/types declarations

  win*       - Windows specific units
  linux*     - Linux specific units
  mac*       - Mac OS X specific units

  *i386*     - Intel x86 CPU specific units (usually x86 types and disassmebler)
  *amd64*    - AMD64 (x86_64) CPU specific units (usually amd64 types and disassembler)

  *dwarf*    - DWARF specific units 
  *stabs*    - Legacy STABS debuf info units
  
more to come:
  bsd* - BSD Unix specifi units
  other processors support

debugger/    - A small command-line debugger, implemented using fpdbglib
  
 
