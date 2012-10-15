{
    fpDebug  -  A debugger for the Free Pascal Compiler.

    Copyright (c) 2012 by Graeme Geldenhuys.

    See the file LICENSE.txt, included in this distribution,
    for details about redistributing fpDebug.

    Description:
      .
}
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

