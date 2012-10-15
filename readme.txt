Free Pascal Debugger
====================
This project is trying to create a Object Pascal written debugger for the
Free Pascal Compiler project. It will read DWARF debug information and be
cross-platform. Initially I will only target Linux and Windows. As soon as I
see good progress, I'll start introducing other platforms too.

This is a fork of the Duby project. The Duby project has stalled in
development, which is a shame. Also my goals are different to Dmitry's,
so instead of bothering him or trying to convince him otherwise, I am going to
continue the work under a new project name. Dmitry is of course wellcome to
pull changes from here.


History
-------
The project originally started a graduation work by Dmitry Boyarintsev back in
2009. His original goal was to create a debugger for Object Pascal based
application, written in Object Pascal, and that can work with Free Pascal and
Delphi 7.


Notes
-----
The project structures is the following

dubylib/     - dir containg all libraries used for 
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

dubyline/    - A small command-line debugger, implemented using dubylib

  
        -------------------  END  ---------------------

