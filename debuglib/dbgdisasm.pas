{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      Abstract disassembler.
}
unit dbgdisasm;

interface

uses
  dbgTypes;

type
  TDisasmElem = class(TObject)
    Instr  : String;
    Params : array [0..3] of String;
    Next   : TDisasmElem;
  end;

  TDisassembler = class(TObject)
  public
    procedure Disasm(const From: TDbgPtr; const Data: array of byte); virtual; abstract;
    function FirstElem: TDisasmElem; virtual; abstract;
  end;
  

implementation


end.
