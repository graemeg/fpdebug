unit winDbgTypes; 

{$mode objfpc}{$H+}

interface

uses
  dbgTypes;
  
type
  { TWinDbgProcess }

  TWinDbgProcess = class(TDbgProcess)
  private
    fState    : TDbgState;
    fCmdLine  : String;
  protected
    function GetProcessState: TDbgState; override;
  public
    constructor Create;
    destructor Destroy; override;
    
    function Execute(const ACommandLine: String): Boolean; override;
    procedure Terminate; override;
    function WaitNextEvent(var Event: TDbgEvent): Boolean; override;
  end;
  

implementation

{ TWinDbgProcess }

constructor TWinDbgProcess.Create; 
begin

end;

destructor TWinDbgProcess.Destroy;  
begin
  inherited Destroy;  
end;

function TWinDbgProcess.GetProcessState: TDbgState;  
begin
  Result := fState;
end;

function TWinDbgProcess.Execute(const ACommandLine: String): Boolean;  
begin
  fCmdLine := ACommandLine;
  fState := ds_ReadToRun;
  Result := true;
end;

procedure TWinDbgProcess.Terminate;  
begin
  if fState in [ds_Nonstarted, ds_Terminated] then Exit;
end;

function TWinDbgProcess.WaitNextEvent(var Event: TDbgEvent): Boolean;  
begin
  Result := false;
end;

end.

