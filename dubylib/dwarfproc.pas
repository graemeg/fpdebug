//todo:
unit dwarfProc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbgTypes, dwarfConst;

type

  { TDwarfMachine }

  TDwarfMachine = class(TObject)
  public
    StackState  : array of Int64;
    procedure Proc(AValue: PByte; Count: Integer);
  end;

implementation

{ TDwarfMachine }

procedure TDwarfMachine.Proc(AValue: PByte; Count: Integer);
begin

end;

end.

