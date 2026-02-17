NOTE: This project has been discontinued, and will be replaced by a brand
      new one. Details to follow later.  -- Graeme


Free Pascal Debugger
====================
This project is trying to create a debugger written in Object Pascal, for the
Free Pascal Compiler project. It will read DWARF debug information only and
be cross-platform. The initially goal is to start by supporting FreeBSD,
Linux and Windows targets. Other platforms can be introduced as soon as we
see good progress with the existing three.


License
-------
This project uses the BSD 2-clause license. Please see the LICENSE.txt for more
details.


History
-------
The project was originally started by Dmitry Boyarintsev back in 2009 under
the name Duby. His original goal was to create a debugger written in Object
Pascal for Object Pascal based applications. He also wanted it to work with
Free Pascal and Delphi 7. The project unfortunately quickly died down, which
was a shame, as it showed potential.


Notes
-----
The project structures is the following

debuglib/     - dir containg all libraries used for
  dbg*       - common debugging unit. Usually contains classes/types declarations

  win*       - Windows specific units
  linux*     - Linux & FreeBSD units
  mac*       - Mac OS X specific units

  *i386*     - Intel x86 CPU specific units (usually x86 types and disassmebler)
  *amd64*    - AMD64 (x86_64) CPU specific units (usually amd64 types and disassembler)

  *dwarf*    - DWARF specific units
  *stabs*    - Legacy STABS debug info units (deprecated)

more to come:
  bsd* - BSD Unix specifi units
  other processors support

debugger/    - A small command-line debugger, implemented using debuglib


        -------------------  END  ---------------------

