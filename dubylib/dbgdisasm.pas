// abstract disassembler
unit dbgdisasm;

interface

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