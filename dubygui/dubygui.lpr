program dubygui;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  mainform,

  dbgDataRead,
  dbgInfoTypes,
  dbgInfoPascal,
  //dbgInfoDwarf,  // dwarf debug information
  dbgInfoStabs,  // stabs debug information

  PESource,        // Win (PE), Linux (elf), MacOSX (macho) executable files
  elfdbgsource,    // the implementation is cross-platform, so there's no need
  machoDbgSource   // to cover them in {$ifdefs}

  {$ifdef mswindows},winDbgTypes{$endif}          // windows debug API
  {$ifdef darwin},macDbgType{$endif} // macosx debug API
  {$ifdef linux},nixDbgTypes{$endif}              // linux debug API

  {$ifdef CPUI386}  ,dbgi386{$endif} // i386 CPU routines
  {$ifdef CPUx86_64},dbgi386{$endif}  //  x64 CPU routines
  ,dbgMain
  ;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

